module Simulation.Aivika.Examples.Model

open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

/// the simulation specs
let specs = {
  StartTime = 0.0
  StopTime = 300.0
  DT = 0.1
  Method = RungeKutta4
  GeneratorType = StrongGenerator
}

/// the mean delay of the input arrivals distributed exponentially
let meanOrderDelay = 0.4

/// the capacity of the queue before the first work places
let queueMaxCount1 = 4

/// the capacity of the queue before the second work places
let queueMaxCount2 = 2

/// the mean processing time distributed exponentially in
/// the first work stations
let meanProcessingTime1 = 0.25

/// the mean processing time distributed exponentially in
/// the second work stations
let meanProcessingTime2 = 0.5

/// the simulation model
let model = simulation {

  // it will gather the statistics about the processing time
  let! arrivalTimer = ArrivalTimer.create

  // define a stream of input events
  let inputStream = Stream.randomExponential meanOrderDelay

  // create a queue in front of the first work stations
  let! queue1 =
    Queue.createUsingFCFS queueMaxCount1
    |> Eventive.runInStartTime

  // create a queue between the first and second work stations
  let! queue2 =
    Queue.createUsingFCFS queueMaxCount2
    |> Eventive.runInStartTime

  // create the first work station (server)
  let! workStation1 =
    Server.createRandomExponential meanProcessingTime1

  // create the second work station (server)
  let! workStation2 =
    Server.createRandomExponential meanProcessingTime2

  // the entire processor from input to output
  let entireProcessor =
    Queue.processorWithLost queue1
    >> Server.processor workStation1
    >> Queue.processor queue2
    >> Server.processor workStation2
    >> ArrivalTimer.processor arrivalTimer

  // start simulating the model
  do! inputStream
    |> entireProcessor
    |> Stream.sink
    |> Proc.runInStartTime

  // return the simulation results
  return [
    ResultSource.From("queue1", queue1, "Queue no. 1");
    ResultSource.From("workStation1", workStation1, "Work Station no. 1");
    ResultSource.From("queue2", queue2, "Queue no. 2");
    ResultSource.From("workStation2", workStation2, "Work Station no. 2");
    ResultSource.From("arrivalTimer", arrivalTimer, "The arrival timer")
  ] |> ResultSet.create
 }

/// the model summary
let modelSummary =
  model |> Simulation.map ResultSet.summary
