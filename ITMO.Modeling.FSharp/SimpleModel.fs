module ITMO.Modeling.FSharp.SimpleModel

open ITMO.Modeling.FSharp.Coefficients
open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

let specs = {
  StartTime = 0.0
  StopTime = 480.0
  DT = 0.1
  Method = RungeKutta4
  GeneratorType = StrongGenerator
}

let createModel coefficients = simulation {
  let inputStream = Stream.randomExponential (1.0 / coefficients.StreamRate)

  let! firstQueue = InfiniteQueue.createUsingFCFS |> Eventive.runInStartTime
  let! firstServer = Server.createRandomExponential coefficients.WorkTime
  
  let! secondQueue = Queue.createUsingFCFS coefficients.Capacity2 |> Eventive.runInStartTime
  let! secondServer = Server.createRandomExponential coefficients.WorkTime

  let! thirdQueue = Queue.createUsingFCFS coefficients.Capacity3 |> Eventive.runInStartTime
  let! thirdServer = Server.createRandomExponential coefficients.WorkTime

  let! arrivalTimer = ArrivalTimer.create

  do! inputStream
    |> InfiniteQueue.processor firstQueue
    |> Server.processor firstServer
    |> Processor.arrc (fun s -> proc {
      let! useSecond = Parameter.randomTrue coefficients.BranchProbability |> Parameter.lift

      if useSecond
      then
        return s
      else
        return s
    })
    |> ArrivalTimer.processor arrivalTimer
    |> Stream.sink
    |> Proc.runInStartTime

  return [
    ResultSource.From("queue", firstQueue, "Queue no. 1")
    ResultSource.From("workStation", firstServer, "Work Station no. 1")
    ResultSource.From("arrivalTimer", arrivalTimer, "The arrival timer")
  ] |> ResultSet.create
 }

let modelSummary =
  createModel personCoefficients
  |> Simulation.map ResultSet.summary
