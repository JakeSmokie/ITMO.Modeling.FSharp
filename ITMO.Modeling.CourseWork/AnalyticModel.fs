module ITMO.Modeling.CourseWork.AnalyticModel

open Simulation.Aivika
open Simulation.Aivika.Queues
open ITMO.Modeling.CourseWork.Base
open Simulation.Aivika.Charting.Gtk.Web
open Simulation.Aivika.Experiments
open Simulation.Aivika.Results

let streamDelay = day / 4.
let hrProbabilty = 0.7
let serveTime = 30.
let runCount = 1000

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

  let! firstHRQueue = infiniteQueue() 
  let! firstManagersQueue = infiniteQueue()

  let! firstManagersServer = parallelServer Server.createRandomExponential serveTime 3
  let! firstHRServer = parallelServer Server.createRandomExponential serveTime 3

  // Choice
  do! (proc {
    for x in stream do
      let! hr = Parameter.lift ^ Parameter.randomTrue hrProbabilty 

      do! if hr then firstHRQueue else firstManagersQueue
          |> InfiniteQueue.enqueue x
          |> Eventive.lift
  } |> Proc.runInStartTime)
  
  do!
    asStream firstManagersQueue
    |> parServerProcessor firstManagersServer
    |> ArrivalTimer.processor timer
    |> run
    
  do!
    asStream firstHRQueue
    |> parServerProcessor firstHRServer
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