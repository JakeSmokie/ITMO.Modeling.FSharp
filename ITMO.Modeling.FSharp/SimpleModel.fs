module ITMO.Modeling.FSharp.SimpleModel

open ITMO.Modeling.FSharp.Coefficients

open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

let specs = {
  StartTime = 0.0
  StopTime = 1200.0
  DT = 0.1
  Method = RungeKutta4
  GeneratorType = StrongGenerator
}

let createModel coefficients = simulation {
  let inputStream = Stream.randomExponential coefficients.StreamDelay

  let! firstQueue =
    InfiniteQueue.createUsingFCFS
    |> Eventive.runInStartTime

  let! firstServer =
    fun _ -> Server.createRandomExponential coefficients.WorkTime
    |> List.init coefficients.ChannelsCount
    |> Simulation.ofList

  let! secondQueue = Queue.createUsingFCFS coefficients.Capacity2 |> Eventive.runInStartTime
  let! secondServer =
    match coefficients.Distributions with
    | BothExponential ->
      Server.createRandomExponential coefficients.WorkTime
    | ConstAndUniform ->
      Server.create (fun a -> proc {
           do! Proc.hold coefficients.WorkTime
           return a
         })
    | ErlangAndUniform | ErlangAndHyper ->
      let m = 2
      let beta = float 2 / coefficients.WorkTime
      
      Server.createRandomErlang beta m
      
  let! thirdQueue = Queue.createUsingFCFS coefficients.Capacity3 |> Eventive.runInStartTime
  let! thirdServer =
    match coefficients.Distributions with
    | BothExponential ->
      Server.createRandomExponential coefficients.WorkTime
    | ConstAndUniform | ErlangAndUniform ->
      let mean = coefficients.WorkTime
      let vc = (sqrt 3.0) * coefficients.VC
      
      Server.createRandomUniform (mean * (1.0 - vc)) (mean * (1.0 + vc))
    | ErlangAndHyper ->
      Server.create (fun x -> proc {        
        let vc = 2.0
        
        let maxProb = 2.0 / (1.0 + vc ** 2.0)
        let! prob = Parameter.randomUniform 0.0 maxProb |> Parameter.lift
        let! first = Parameter.randomTrue prob |> Parameter.lift

        let mean = coefficients.WorkTime
        let mean =
          if first
          then mean * (1.0 + sqrt ((1.0 - prob) / (2.0 * prob) * (vc ** 2.0 - 1.0)))
          else mean * (1.0 + sqrt (prob / (2.0 * (1.0 - prob)) * (vc ** 2.0 - 1.0)))
          
        do! Proc.randomExponential_ mean
        return x
      })
  
  let! arrivalTimer = ArrivalTimer.create

  do! (proc {
    let stream =
      inputStream
      |> InfiniteQueue.processor firstQueue
      |> Processor.par (List.map Server.processor firstServer)

    for x in stream do
      let! second =
        Parameter.randomTrue coefficients.BranchProbability
        |> Parameter.lift

      if second then
        do! secondQueue
            |> Queue.enqueueOrLost_ x
            |> Eventive.lift
      else
        do! thirdQueue
            |> Queue.enqueueOrLost_ x
            |> Eventive.lift
  } |> Proc.runInStartTime)

  do!
    secondQueue
    |> Queue.dequeue
    |> Stream.repeat
    |> Server.processor secondServer
    |> ArrivalTimer.processor arrivalTimer
    |> Stream.sink
    |> Proc.runInStartTime

  do!
    thirdQueue
    |> Queue.dequeue
    |> Stream.repeat
    |> Server.processor thirdServer
    |> ArrivalTimer.processor arrivalTimer
    |> Stream.sink
    |> Proc.runInStartTime

  let firstServerTime = firstServer |> List.map Server.processingTime
  let secondServerTime = secondServer |> Server.processingTime
  let thirdServerTime = thirdServer |> Server.processingTime

  let firstSectionTime =
    firstQueue
    |> InfiniteQueue.waitTime
    |> Eventive.map (fun waitTime -> eventive {
      let! firstServerTime =
        firstServerTime
        |> Eventive.ofList
        |> Eventive.map (List.fold SamplingStats.append SamplingStats.emptyFloats)

      return (SamplingStats.mean firstServerTime) + (SamplingStats.mean waitTime)
    })
    |> Eventive.concat
    |> Eventive.run

  let secondSectionTime =
    secondQueue
    |> Queue.waitTime
    |> Eventive.map (fun waitTime -> eventive {
      let! time = secondServerTime
      return (SamplingStats.mean time) + (SamplingStats.mean waitTime)
    })
    |> Eventive.concat
    |> Eventive.run

  let thirdSectionTime =
    thirdQueue
    |> Queue.waitTime
    |> Eventive.map (fun waitTime -> eventive {
      let! time = thirdServerTime
      return (SamplingStats.mean time) + (SamplingStats.mean waitTime)
    })
    |> Eventive.concat
    |> Eventive.run

  let secondQueueLossProb =
    Queue.lostCount secondQueue
    |> Eventive.map (fun lostCount -> eventive {
      let! count = Queue.outputCount secondQueue
      let sum = lostCount + count
      let var = if sum = 0 then 0.0 else (float lostCount) / (float sum)

      return var
    })
    |> Eventive.concat
    |> Eventive.run

  let thirdQueueLossProb =
    Queue.lostCount thirdQueue
    |> Eventive.map (fun lostCount -> eventive {
      let! count = Queue.outputCount thirdQueue
      let sum = lostCount + count
      let var = if sum = 0 then 0.0 else (float lostCount) / (float sum)

      return var
    })
    |> Eventive.concat
    |> Eventive.run

  let firstServerLoad =
    firstServer |> List.map (fun server ->
      Server.processingTime server
      |> Eventive.map (fun time -> (SamplingStats.mean time) / coefficients.StreamDelay)
      |> Eventive.run
    )

  let secondServerLoad =
    Server.processingTime secondServer
    |> Eventive.map(fun time ->
      (SamplingStats.mean time) * coefficients.BranchProbability / coefficients.StreamDelay
    ) |> Eventive.run

  let thirdServerLoad =
    Server.processingTime thirdServer
    |> Eventive.map(fun time ->
      (SamplingStats.mean time) * (1.0 - coefficients.BranchProbability) / coefficients.StreamDelay
    ) |> Eventive.run

  let arrivalVarianceCoefficient =
    ArrivalTimer.processingTime arrivalTimer
    |> Eventive.map (fun time ->
      (SamplingStats.deviation time) / (SamplingStats.mean time)
    ) |> Eventive.run

  return [
    ResultSource.From("queue 1", firstQueue)
    ResultSource.From("queue 2", secondQueue)
    ResultSource.From("queue 3", thirdQueue)

    ResultSource.From("queue 2 loss prob", secondQueueLossProb)
    ResultSource.From("queue 3 loss prob", thirdQueueLossProb)

    ResultSource.From("server 1", firstServer)
    ResultSource.From("server 2", secondServer)
    ResultSource.From("server 3", thirdServer)

    ResultSource.From("server 1 load", firstServerLoad)
    ResultSource.From("server 2 load", secondServerLoad)
    ResultSource.From("server 3 load", thirdServerLoad)

    ResultSource.From("server 1 time", firstServerTime)
    ResultSource.From("server 2 time", secondServerTime)
    ResultSource.From("server 3 time", thirdServerTime)

    ResultSource.From("queue + server 1 time", firstSectionTime)
    ResultSource.From("queue + server 2 time", secondSectionTime)
    ResultSource.From("queue + server 3 time", thirdSectionTime)

    ResultSource.From("arrivalTimer", arrivalTimer)
    ResultSource.From("arrivalTimerVC", arrivalVarianceCoefficient)
  ] |> ResultSet.create
 }

let modelSummary =
  createModel personCoefficients
  |> Simulation.map ResultSet.summary
