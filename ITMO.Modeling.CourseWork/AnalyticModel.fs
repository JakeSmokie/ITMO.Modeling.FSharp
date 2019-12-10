module ITMO.Modeling.CourseWork.AnalyticModel

open Simulation.Aivika
open Simulation.Aivika.Queues
open ITMO.Modeling.CourseWork.Base
open Simulation.Aivika.Charting.Gtk.Web
open Simulation.Aivika.Experiments
open Simulation.Aivika.Results

let streamDelay = days 1 / 100.0
let hrProbability = 0.7
let serveTime = 30.0
let runCount = 100

let specs = {
  StartTime = 0.0
  StopTime = 1440. * 14.
  DT = 1.
  Method = RungeKutta4
  GeneratorType = StrongGenerator
}

let model = simulation {
  let! timer = ArrivalTimer.create
  let stream = Stream.randomExponential streamDelay

  let! firstHRQueue = queue
  let! firstManagersQueue = queue

  let! firstManagersServer = createServer 3 ^ fun _ -> Server.createRandomExponential serveTime
  let! firstHRServer = createServer 3 ^ fun _ -> Server.createRandomExponential serveTime

  // Choice
  do! (proc {
    for x in stream do
      let! hr = Parameter.lift ^ Parameter.randomTrue hrProbability 

      do! if hr then firstHRQueue else firstManagersQueue
          |> InfiniteQueue.enqueue x
          |> Eventive.lift
  } |> Proc.runInStartTime)
  
  do!
    dequeue firstManagersQueue
    |> serve firstManagersServer
    |> ArrivalTimer.processor timer
    |> run
    
  do!
    dequeue firstHRQueue
    |> serve firstHRServer
    |> ArrivalTimer.processor timer
    |> run
  
  return [
    ResultSource.From("firstHRQueue", firstHRQueue)
    ResultSource.From("firstManagersQueue", firstManagersQueue)
    ResultSource.From("firstHRServer", firstHRServer)
    ResultSource.From("firstManagersServer", firstManagersServer)
    ResultSource.From("timer", timer)
  ] |> ResultSet.create
}

let run() =
  let experiment = Experiment()

  experiment.Specs <- specs
  experiment.RunCount <- runCount

  let firstHRQueue = ResultSet.findByName "firstHRQueue"
  let firstManagersQueue = ResultSet.findByName "firstManagersQueue"
  let firstHRServer = ResultSet.findByName "firstHRServer"
  let firstManagersServer = ResultSet.findByName "firstManagersServer"
  let timer = ResultSet.findByName "timer"
  
  let providers = [
    ExperimentProvider.infiniteQueue firstHRQueue
    ExperimentProvider.infiniteQueue firstManagersQueue
    
    ExperimentProvider.server firstHRServer
    ExperimentProvider.server firstManagersServer
    
    ExperimentProvider.arrivalTimer timer 
  ]
  
  experiment.RenderHtml(model, providers)
  |> Async.RunSynchronously