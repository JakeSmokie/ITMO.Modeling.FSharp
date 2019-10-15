module ITMO.Modeling.FSharp.SimpleModel

open ITMO.Modeling.FSharp.Coefficients
open Simulation.Aivika
open Simulation.Aivika
open Simulation.Aivika
open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

let specs = {
  StartTime = 0.0
  StopTime = 1000.0
  DT = 0.1
  Method = RungeKutta4
  GeneratorType = StrongGenerator
}

let createModel coefficients = simulation {
  let inputStream = Stream.randomExponential coefficients.StreamComingTime

  let! firstQueue =
    InfiniteQueue.createUsingFCFS
    |> Eventive.runInStartTime

  let! firstServer =
    fun _ -> Server.createRandomExponential coefficients.WorkTime
    |> List.init coefficients.ChannelsCount
    |> Simulation.ofList

  let! secondQueue = Queue.createUsingFCFS coefficients.Capacity2 |> Eventive.runInStartTime
  let! secondServer = Server.createRandomExponential coefficients.WorkTime

  let! thirdQueue = Queue.createUsingFCFS coefficients.Capacity3 |> Eventive.runInStartTime
  let! thirdServer = Server.createRandomExponential coefficients.WorkTime

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
  
  return [
    ResultSource.From("queue 1", firstQueue, "Queue no. 1")
    ResultSource.From("queue 2", secondQueue, "Queue no. 2")
    ResultSource.From("queue 3", thirdQueue, "Queue no. 3")

    ResultSource.From("server 1", firstServer, "Work Station no. 1")
    ResultSource.From("server 2", secondServer, "Work Station no. 2")
    ResultSource.From("server 3", thirdServer, "Work Station no. 3")

    ResultSource.From("server 1 time", firstServerTime, "Work Station no. 1")
    ResultSource.From("server 2 time", secondServerTime, "Work Station no. 2")
    ResultSource.From("server 3 time", thirdServerTime, "Work Station no. 3")
    
    ResultSource.From("arrivalTimer", arrivalTimer, "The arrival timer")
  ] |> ResultSet.create
 }

let modelSummary =
  createModel personCoefficients
  |> Simulation.map ResultSet.summary
