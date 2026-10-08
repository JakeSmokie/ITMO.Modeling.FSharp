module Portfolio.Tests
open System
open Xunit
open Portfolio
let private baseline = Simulation.defaults
let private rejects c =
    Assert.Throws<ArgumentException>(fun () -> Simulation.run c 0 |> ignore) |> ignore
[<Fact>]
let ZeroArrivalsLeaveSystemEmpty () =
    let r = Simulation.run {baseline with ArrivalPerHour=0.} 0
    Assert.True(r.Arrived=0 && r.Completed=0 && r.Lost=0 && r.Pending=0)
    Assert.True(r.Nodes |> Array.forall (fun n -> n.Utilization=0. && n.AverageQueue=0.))
[<Fact>]
let EveryRunConservesJobs () =
    for seed in 1..40 do
        for distribution in ["constant";"exponential";"erlang"] do
            let r = Simulation.run {baseline with Seed=seed; ArrivalPerHour=110.; Distribution=distribution} 0
            Assert.Equal(r.Arrived,r.Completed+r.Lost+r.Pending)
[<Fact>]
let SameInputsReplayExactly () =
    Assert.True(Simulation.run baseline 0 = Simulation.run baseline 0)
[<Fact>]
let ReplicasUseDifferentStreams () =
    Assert.False(Simulation.run baseline 0 = Simulation.run baseline 1)
[<Fact>]
let FiniteQueuesRespectCapacity () =
    for capacity in [0;1;5;100] do
        let r = Simulation.run {baseline with QueueCapacity=capacity; ArrivalPerHour=180.} 0
        Assert.True(r.Nodes[1..] |> Array.forall (fun n -> n.MaxQueue <= capacity))
[<Fact>]
let UtilizationRespectsPhysicalCapacity () =
    for seed in 1..30 do
        let r = Simulation.run {baseline with Seed=seed; ArrivalPerHour=200.} 0
        Assert.True(r.Nodes |> Array.forall (fun n -> n.Utilization >= 0. && n.Utilization <= 1.0000000001))
[<Fact>]
let ProbabilityZeroSkipsBranchA () =
    let r = Simulation.run {baseline with BranchProbability=0.} 0
    Assert.Equal(0,r.Nodes[1].Started)
    Assert.Equal(0,r.Nodes[1].Rejected)
[<Fact>]
let ProbabilityOneSkipsBranchB () =
    let r = Simulation.run {baseline with BranchProbability=1.} 0
    Assert.Equal(0,r.Nodes[2].Started)
[<Fact>]
let FullBufferRejectsJobs () =
    let r = Simulation.run {baseline with ArrivalPerHour=120.; IntakeWorkers=8; QueueCapacity=0} 0
    Assert.True(r.Lost>0)
    Assert.Equal(r.Lost,r.Nodes[1].Rejected+r.Nodes[2].Rejected)
[<Fact>]
let ArrivalsAreIndependentOfStaffing () =
    let a = Simulation.run baseline 0
    let b = Simulation.run {baseline with BranchWorkers=4; IntakeWorkers=5} 0
    Assert.Equal(a.Arrived,b.Arrived)
[<Fact>]
let SamplingIncludesHorizonEnds () =
    let r = Simulation.run baseline 0
    Assert.Equal(49,r.Timeline.Length)
    Assert.Equal(0.,r.Timeline[0].Hour)
    Assert.Equal(baseline.DurationHours,r.Timeline[48].Hour)
[<Fact>]
let CumulativeCountersNeverDecrease () =
    let r = Simulation.run baseline 0
    Assert.True(r.Timeline |> Array.pairwise |> Array.forall (fun (a,b) -> b.Completed >= a.Completed && b.Lost >= a.Lost))
[<Fact>]
let SummaryPreservesJobAccounting () =
    let r = Simulation.experiment baseline
    Assert.True(abs(r.Arrived-r.Completed-r.Lost-r.Pending)<1e-9)
    Assert.True(float r.CompletedMin <= r.Completed && r.Completed <= float r.CompletedMax)
[<Theory>]
[<InlineData(Double.NaN)>]
[<InlineData(Double.PositiveInfinity)>]
[<InlineData(-1.)>]
[<InlineData(241.)>]
let InvalidArrivalRatesAreRejected value = rejects {baseline with ArrivalPerHour=value}
[<Theory>]
[<InlineData(0)>]
[<InlineData(9)>]
let WorkersAreBounded value = rejects {baseline with IntakeWorkers=value}
[<Fact>]
let ZeroServiceIsRejected () = rejects {baseline with ServiceMinutes=0.}
[<Fact>]
let InvalidProbabilityIsRejected () = rejects {baseline with BranchProbability=1.1}
[<Fact>]
let ComputationBudgetIsBounded () =
    rejects {baseline with Replications=25}
    rejects {baseline with DurationHours=25.}
[<Fact>]
let UnsupportedDistributionIsRejected () = rejects {baseline with Distribution="other"}
[<Fact>]
let JsonContractHasFiniteNumbersAndCamelCase () =
    let json = """{"arrivalPerHour":0,"serviceMinutes":4,"intakeWorkers":2,"branchWorkers":1,"queueCapacity":6,"branchProbability":0.5,"durationHours":8,"distribution":"exponential","seed":42,"replications":2}"""
    let result = Api.Run("simulate",json)
    Assert.Contains("\"completed\":0",result)
    Assert.DoesNotContain("NaN",result)

[<Fact>]
let SummaryCountersAreMeansAcrossAllReplicas () =
    let c = {baseline with ArrivalPerHour=110.; Replications=12}
    let runs = Array.init c.Replications (Simulation.run c)
    let summary = Simulation.experiment c
    for i in 0..2 do
        Assert.Equal(runs |> Array.averageBy (fun r -> float r.Nodes[i].Started),summary.Nodes[i].Started)
        Assert.Equal(runs |> Array.averageBy (fun r -> float r.Nodes[i].Rejected),summary.Nodes[i].Rejected)
    Assert.True(abs(summary.Lost - (summary.Nodes |> Array.sumBy (fun n -> n.Rejected))) < 1e-9)
