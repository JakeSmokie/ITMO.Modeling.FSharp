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
  StopTime = 480.0
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
    List.init coefficients.ChannelsCount (fun _ -> Server.createRandomExponential coefficients.WorkTime)
    |> Simulation.ofList
    
  let! secondQueue = Queue.createUsingFCFS coefficients.Capacity2 |> Eventive.runInStartTime
  let! secondServer = Server.createRandomExponential coefficients.WorkTime

  let! thirdQueue = Queue.createUsingFCFS coefficients.Capacity3 |> Eventive.runInStartTime
  let! thirdServer = Server.createRandomExponential coefficients.WorkTime
  
  let! arrivalTimer = ArrivalTimer.create

  let k =
    inputStream
    |> InfiniteQueue.processor firstQueue
    |> Processor.par (List.map (Server.processor) firstServer)
    |> ArrivalTimer.processor arrivalTimer
  
  do! k
    |> Processor.arrc (fun x -> proc {
      let! useSecond = Parameter.randomTrue coefficients.BranchProbability |> Parameter.lift
      
      if useSecond
      then ()
      else ()
      
      return x
    })
    |> Stream.sink
    |> Proc.runInStartTime

  let serversProcessingTime =
    firstServer
    |> List.map Server.totalProcessingTime
    |> Eventive.ofList
    |> Eventive.map List.average
  
  return [
    ResultSource.From("queue", firstQueue, "Queue no. 1")
    ResultSource.From("avg", serversProcessingTime, "avg")
    ResultSource.From("workStation", firstServer, "Work Station no. 1")
    ResultSource.From("arrivalTimer", arrivalTimer, "The arrival timer")
  ] |> ResultSet.create
 }

let modelSummary =
  createModel personCoefficients
  |> Simulation.map ResultSet.summary
