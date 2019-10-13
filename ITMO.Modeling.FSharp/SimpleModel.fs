module ITMO.Modeling.FSharp.SimpleModel

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

let model = simulation {
  let! arrivalTimer = ArrivalTimer.create

  let inputStream = Stream.randomUniform 12.0 24.0
  let! workStation = Server.createRandomUniform 12.0 20.0
  
  let! queue =
    InfiniteQueue.createUsingFCFS
    |> Eventive.runInStartTime
  
  do! inputStream
    |> InfiniteQueue.processor queue
    |> Server.processor workStation
    |> ArrivalTimer.processor arrivalTimer
    |> Stream.sink
    |> Proc.runInStartTime
  
  return [
    ResultSource.From("queue", queue, "Queue no. 1")
    ResultSource.From("workStation", workStation, "Work Station no. 1")
    ResultSource.From("arrivalTimer", arrivalTimer, "The arrival timer")
  ] |> ResultSet.create
}

let modelSummary =
  model |> Simulation.map ResultSet.summary
