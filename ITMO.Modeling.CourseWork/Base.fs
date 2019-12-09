module ITMO.Modeling.CourseWork.Base

open Simulation.Aivika

let (^) f x = f x

let day = 1440.

let parallelServer method serveTime amount =
  fun _ -> method serveTime
  |> List.init amount
  |> Simulation.ofList

let parServerProcessor server = Processor.par ^ List.map Server.processor server

let asStream queue =
  queue
  |> InfiniteQueue.dequeue
  |> Stream.repeat

let run stream =
  stream
  |> Stream.sink
  |> Proc.runInStartTime

let infiniteQueue() = Eventive.runInStartTime ^ InfiniteQueue.createUsingFCFS
