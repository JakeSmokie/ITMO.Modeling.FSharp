module ITMO.Modeling.CourseWork.Base

open Simulation.Aivika

let (^) f x = f x

let minutes x = float x
let hours x = minutes 60 * float x
let days x = hours 24 * float x

let createServer method serveTime amount =
  fun _ -> method serveTime
  |> List.init amount
  |> Simulation.ofList

let serve server = Processor.par ^ List.map Server.processor server

let dequeue queue =
  queue
  |> InfiniteQueue.dequeue
  |> Stream.repeat

let run stream =
  stream
  |> Stream.sink
  |> Proc.runInStartTime

let queue<'a> = Eventive.runInStartTime ^ InfiniteQueue.createUsingFCFS<'a>

let goto queue =
  Processor.arrc ^ fun x -> proc {
    do! Eventive.lift ^ InfiniteQueue.enqueue x queue
  }

let choose trueQueue falseQueue prob =
  Processor.arrc ^ fun x -> proc {
    let! isTrue = Parameter.lift ^ Parameter.randomTrue prob
    
    do! if isTrue then trueQueue else falseQueue
        |> InfiniteQueue.enqueue x
        |> Eventive.lift
    
    return x
  }
