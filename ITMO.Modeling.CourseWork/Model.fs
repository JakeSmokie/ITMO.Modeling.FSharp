module ITMO.Modeling.CourseWork.Model

open Simulation.Aivika
open Simulation.Aivika.Queues
open ITMO.Modeling.CourseWork.Base

type Coefficients = {
  StreamDelay: float
  HRProbability: float
}

let parameters = {
  StreamDelay = day / 4. // every 6 hours
  HRProbability = 0.7
}

let specs = {
  StartTime = 0.0
  StopTime = 1440. * 14.
  DT = 1.
  Method = RungeKutta4
  GeneratorType = StrongGenerator
}

let model = simulation {
  let! timer = ArrivalTimer.create
  let stream = Stream.randomExponential parameters.StreamDelay
  
  let! firstHRQueue = InfiniteQueue.createUsingFCFS |> Eventive.runInStartTime
  let! firstManagersQueue = InfiniteQueue.createUsingFCFS |> Eventive.runInStartTime
  
  // First section
  do! (proc {
    for x in stream do
      let! hr =
        Parameter.randomTrue parameters.HRProbability
        |> Parameter.lift

      if hr then
        do! firstHRQueue
            |> InfiniteQueue.enqueue x
            |> Eventive.lift
      else
        do! firstManagersQueue
            |> InfiniteQueue.enqueue x
            |> Eventive.lift
  } |> Proc.runInStartTime)

  let! firstManagersServer = Server.createRandomExponential 30.
  
  do!
    firstManagersQueue
    |> InfiniteQueue.dequeue
    |> Stream.repeat
    |> Server.processor firstManagersServer
    |> Processor.arrc (fun x -> proc {
      do! Proc.randomExponential_ ^ 7. * day
      return x
    })
    |> InfiniteQueue.processor firstHRQueue
    |> Stream.sink
    |> Proc.runInStartTime
  
  let! firstHRServer = Server.createRandomExponential 30.0
  
  ()
}