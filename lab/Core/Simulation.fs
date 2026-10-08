namespace Portfolio

open System
open System.Collections.Generic

[<CLIMutable>]
type Config = {
    ArrivalPerHour: float
    ServiceMinutes: float
    IntakeWorkers: int
    BranchWorkers: int
    QueueCapacity: int
    BranchProbability: float
    DurationHours: float
    Distribution: string
    Seed: int
    Replications: int
}
type NodeResult = {
    Name: string; Utilization: float; AverageQueue: float
    MeanWait: float; MaxQueue: int; Started: int; Rejected: int
}
type NodeSummary = { Name: string; Utilization: float; AverageQueue: float; MeanWait: float; MaxQueue: int; Started: float; Rejected: float }
type Tick = { Hour: float; Intake: int; BranchA: int; BranchB: int; Completed: int; Lost: int }
type RunResult = {
    Arrived: int; Completed: int; Lost: int; Pending: int
    MeanTime: float; Nodes: NodeResult array; Timeline: Tick array
}
type Summary = {
    Config: Config; Arrived: float; Completed: float; Lost: float; Pending: float
    MeanTime: float; Throughput: float; LossPercent: float
    CompletedMin: int; CompletedMax: int; Nodes: NodeSummary array
    Sample: RunResult
}
type private Job = { Born: float; Queued: float }
type private Event = Arrival | Done of int * Job | Sample

/// Local, explicitly specified PRNG: reproducible across .NET/native/browser versions.
type internal RandomStream(seed: uint32) =
    let mutable state = if seed = 0u then 0x6D2B79F5u else seed
    member _.Next() =
        state <- state ^^^ (state <<< 13)
        state <- state ^^^ (state >>> 17)
        state <- state ^^^ (state <<< 5)
        (float state + 1.) / 4294967297.
    member this.Exponential(mean: float) = -mean * log (1. - this.Next())

module Simulation =
    let defaults = {
        ArrivalPerHour = 20.; ServiceMinutes = 4.; IntakeWorkers = 2
        BranchWorkers = 1; QueueCapacity = 6; BranchProbability = 0.5
        DurationHours = 8.; Distribution = "exponential"; Seed = 42; Replications = 12
    }

    let validate c =
        let finite name low high value =
            if not (Double.IsFinite value) || value < low || value > high then
                invalidArg name (sprintf "%s: допустимый диапазон %g–%g." name low high)
        finite "Поток заявок в час" 0. 240. c.ArrivalPerHour
        finite "Среднее обслуживание, мин" 0.25 30. c.ServiceMinutes
        finite "Вероятность первой ветви" 0. 1. c.BranchProbability
        finite "Длительность, часов" 1. 24. c.DurationHours
        if c.IntakeWorkers < 1 || c.IntakeWorkers > 8 || c.BranchWorkers < 1 || c.BranchWorkers > 8 then
            invalidArg "workers" "Число исполнителей должно быть от 1 до 8."
        if c.QueueCapacity < 0 || c.QueueCapacity > 100 then invalidArg "capacity" "Буфер очереди: от 0 до 100."
        if c.Seed < 1 then invalidArg "seed" "Seed должен быть положительным целым числом."
        if c.Replications < 1 || c.Replications > 24 then invalidArg "replications" "Число прогонов: от 1 до 24."
        if not (List.contains c.Distribution ["exponential"; "constant"; "erlang"]) then
            invalidArg "distribution" "Неизвестное распределение обслуживания."

    let run (c: Config) replica : RunResult =
        validate c
        if replica < 0 || replica > 1000 then invalidArg "replica" "Неверный номер прогона."
        let seed = uint32 c.Seed ^^^ (uint32 (replica + 1) * 0x9E3779B9u)
        // Independent streams keep arrivals and routing comparable when staff changes.
        let arrivalRandom = RandomStream(seed)
        let routeRandom = RandomStream(seed ^^^ 0xA5A5A5A5u)
        let serviceRandom = [| for i in 1..3 -> RandomStream(seed ^^^ (uint32 i * 0x85EBCA6Bu)) |]
        let horizon = c.DurationHours * 60.
        let workers = [|c.IntakeWorkers; c.BranchWorkers; c.BranchWorkers|]
        let busy = Array.zeroCreate<int> 3
        let queues = Array.init 3 (fun _ -> Queue<Job>())
        let queueArea = Array.zeroCreate<float> 3
        let busyArea = Array.zeroCreate<float> 3
        let waited = Array.zeroCreate<float> 3
        let started = Array.zeroCreate<int> 3
        let maxQueue = Array.zeroCreate<int> 3
        let rejected = Array.zeroCreate<int> 3
        let calendar = PriorityQueue<Event, struct(float * int)>()
        let ticks = ResizeArray<Tick>()
        let mutable serial = 0
        let mutable arrived = 0
        let mutable completed = 0
        let mutable lost = 0
        let mutable totalTime = 0.
        let mutable lastTime = 0.
        let schedule time event =
            if not (Double.IsFinite time) then invalidOp "Время события должно быть конечным."
            serial <- serial + 1
            calendar.Enqueue(event, struct(time, serial))
        let serviceDuration node =
            match c.Distribution with
            | "constant" -> c.ServiceMinutes
            | "erlang" ->
                serviceRandom[node].Exponential(c.ServiceMinutes / 2.) +
                serviceRandom[node].Exponential(c.ServiceMinutes / 2.)
            | _ -> serviceRandom[node].Exponential(c.ServiceMinutes)
        let start now node job =
            busy[node] <- busy[node] + 1
            started[node] <- started[node] + 1
            waited[node] <- waited[node] + now - job.Queued
            schedule (now + serviceDuration node) (Done(node, job))
        let admit now node job =
            let job = {job with Queued = now}
            if busy[node] < workers[node] then start now node job
            elif node > 0 && queues[node].Count >= c.QueueCapacity then
                lost <- lost + 1
                rejected[node] <- rejected[node] + 1
            else
                queues[node].Enqueue job
                maxQueue[node] <- max maxQueue[node] queues[node].Count
        let integrate now =
            for i in 0..2 do
                queueArea[i] <- queueArea[i] + float queues[i].Count * (now - lastTime)
                busyArea[i] <- busyArea[i] + float busy[i] * (now - lastTime)
            lastTime <- now
        if c.ArrivalPerHour > 0. then
            schedule (arrivalRandom.Exponential(60. / c.ArrivalPerHour)) Arrival
        for i in 0..48 do schedule (horizon * float i / 48.) Sample
        let mutable executing = true
        while executing && calendar.Count > 0 do
            let mutable event = Unchecked.defaultof<Event>
            let mutable priority = Unchecked.defaultof<struct(float * int)>
            if calendar.TryDequeue(&event, &priority) then
                let struct(now, _) = priority
                if now > horizon then executing <- false
                else
                    integrate now
                    match event with
                    | Arrival ->
                        arrived <- arrived + 1
                        admit now 0 {Born = now; Queued = now}
                        schedule (now + arrivalRandom.Exponential(60. / c.ArrivalPerHour)) Arrival
                    | Done(node, job) ->
                        busy[node] <- busy[node] - 1
                        if node = 0 then
                            admit now (if routeRandom.Next() < c.BranchProbability then 1 else 2) job
                        else
                            completed <- completed + 1
                            totalTime <- totalTime + now - job.Born
                        if queues[node].Count > 0 then start now node (queues[node].Dequeue())
                    | Sample ->
                        ticks.Add {Hour = now / 60.; Intake = queues[0].Count; BranchA = queues[1].Count
                                   BranchB = queues[2].Count; Completed = completed; Lost = lost}
        integrate horizon
        let pending = Array.sum busy + (queues |> Array.sumBy (fun q -> q.Count))
        if arrived <> completed + lost + pending then invalidOp "Нарушен баланс заявок."
        { Arrived = arrived; Completed = completed; Lost = lost; Pending = pending
          MeanTime = if completed = 0 then 0. else totalTime / float completed
          Nodes = Array.init 3 (fun i ->
              { Name = [|"Приём"; "Ветвь A"; "Ветвь B"|][i]
                Utilization = busyArea[i] / (horizon * float workers[i])
                AverageQueue = queueArea[i] / horizon
                MeanWait = if started[i] = 0 then 0. else waited[i] / float started[i]
                MaxQueue = maxQueue[i]; Started = started[i]; Rejected = rejected[i] })
          Timeline = ticks.ToArray() }

    let experiment (c: Config) : Summary =
        validate c
        let runs = Array.init c.Replications (run c)
        let mean f = runs |> Array.averageBy f
        let arrived = mean (fun r -> float r.Arrived)
        let lost = mean (fun r -> float r.Lost)
        let completed = mean (fun r -> float r.Completed)
        { Config = c; Arrived = arrived; Completed = completed; Lost = lost
          Pending = mean (fun r -> float r.Pending)
          MeanTime = if completed = 0. then 0. else
                        (runs |> Array.sumBy (fun r -> r.MeanTime * float r.Completed)) /
                        (runs |> Array.sumBy (fun r -> float r.Completed))
          Throughput = completed / c.DurationHours
          LossPercent = if arrived = 0. then 0. else lost / arrived * 100.
          CompletedMin = runs |> Array.minBy (fun r -> r.Completed) |> fun r -> r.Completed
          CompletedMax = runs |> Array.maxBy (fun r -> r.Completed) |> fun r -> r.Completed
          Nodes = Array.init 3 (fun i ->
              let sample = runs[0].Nodes[i]
              let count = runs |> Array.sumBy (fun r -> r.Nodes[i].Started)
              { Name = sample.Name
                Utilization = mean (fun r -> r.Nodes[i].Utilization)
                AverageQueue = mean (fun r -> r.Nodes[i].AverageQueue)
                MeanWait =
                    if count = 0 then 0. else
                        (runs |> Array.sumBy (fun r -> r.Nodes[i].MeanWait * float r.Nodes[i].Started)) / float count
                Started = mean (fun r -> float r.Nodes[i].Started)
                Rejected = mean (fun r -> float r.Nodes[i].Rejected)
                MaxQueue = runs |> Array.maxBy (fun r -> r.Nodes[i].MaxQueue) |> fun r -> r.Nodes[i].MaxQueue })
          Sample = runs[0] }
