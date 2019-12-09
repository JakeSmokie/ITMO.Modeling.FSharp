module ITMO.Modeling.CourseWork.ComplexModel

open Simulation.Aivika
open Simulation.Aivika.Charting.Gtk.Web
open Simulation.Aivika.Experiments
open Simulation.Aivika.Results
open ITMO.Modeling.CourseWork.Base
open Simulation.Aivika.Experiments.Web

let streamDelay = days 1 / 4.
let cvTransferServingTime = minutes 10

let managersAmount = 100
let hrAmount = 4

let model = simulation {
  let! timer = ArrivalTimer.create
  let! successTimer = ArrivalTimer.create
  let! failureTimer = ArrivalTimer.create
  
  let stream = Stream.randomExponential streamDelay

  let! firstManagersQueue = queue
  let! firstHRQueue = queue 
  
  let! secondManagersQueue = queue
  let! secondHRQueue = queue
  
  let! firstInterviewQueue = queue
  let! secondInterviewQueue = queue
  
  let! failureQueue = queue
  let! successQueue = queue
  
  let! managersServer = createServer Server.createRandomExponential cvTransferServingTime managersAmount
  let! hrServer = createServer Server.createRandomExponential cvTransferServingTime hrAmount

  // Util
  let try_ success prob = choose success failureQueue prob

  // Here goes action
  // M or HR
  do!
    stream
    |> choose firstHRQueue firstManagersQueue 0.9
    |> run
  
  // Manager -> HRs
  do!
    dequeue firstManagersQueue
    |> serve managersServer
    |> Processor.randomExponential ^ days 1 // delay
    |> goto firstHRQueue
    |> run
  
  // HRs
  let scale = 7
  let mean = hours 3
  
  do!
    dequeue firstHRQueue
    |> serve hrServer
    |> Processor.randomErlang (float scale / mean) scale // delay
    |> try_ secondManagersQueue 0.4
    |> run
  
  // Managers
  do!
    dequeue secondManagersQueue
    |> serve managersServer
    |> Processor.randomExponential ^ days 1 // delay
    |> try_ secondHRQueue 0.4
    |> run
    
  do!
    dequeue secondHRQueue
    |> serve hrServer
    |> Processor.randomExponential ^ days 7 // delay
    |> try_ firstInterviewQueue 0.4
    |> run
  
  do!
    dequeue firstInterviewQueue
    |> serve hrServer
    |> serve managersServer
    |> Processor.arrc ^ fun x -> proc {
      let! success = Parameter.lift ^ Parameter.randomTrue 0.5
      let! secondInterview = Parameter.lift ^ Parameter.randomTrue 0.1

      let queue =
        if not success then
          failureQueue           
        else
          if secondInterview
          then secondInterviewQueue
          else successQueue
            
      do!
        queue
        |> InfiniteQueue.enqueue x
        |> Eventive.lift 
          
      return x
    }
    |> run
  
  do!
    dequeue secondInterviewQueue
    |> serve hrServer
    |> serve managersServer
    |> try_ successQueue 0.5
    |> run
  
  // Failure
  do!
    dequeue failureQueue
    |> ArrivalTimer.processor timer
    |> ArrivalTimer.processor failureTimer
    |> run

  // Success
  do!
    dequeue successQueue
    |> ArrivalTimer.processor timer
    |> ArrivalTimer.processor successTimer
    |> run

    
  return [
    ResultSource.From("firstHRQueue", firstHRQueue)
    ResultSource.From("firstManagersQueue", firstManagersQueue)
    ResultSource.From("secondManagersQueue", secondManagersQueue)
    ResultSource.From("hrServer", hrServer)
    ResultSource.From("managersServer", managersServer)
    ResultSource.From("timer", timer)
    ResultSource.From("successTimer", successTimer)
    ResultSource.From("failureTimer", failureTimer)
  ] |> ResultSet.create
}

let specs = {
  StartTime = 0.0
  StopTime = days 365
  DT = minutes 1
  Method = RungeKutta4
  GeneratorType = StrongGenerator
}

let serverProvider series =
  let series1 = series >> ResultSet.findById ServerProcessingFactorId

  [
    ExperimentProvider.deviationChart series1
    ExperimentProvider.lastValueStats series1
    ExperimentProvider.lastValueHistogram series1
  ] |> ExperimentProvider.concat

let runCount = 10
let run() =
  let experiment = Experiment()

  experiment.Specs <- specs
  experiment.RunCount <- runCount

  let firstHRQueue = ResultSet.findByName "firstHRQueue"
  let firstManagersQueue = ResultSet.findByName "firstManagersQueue"
  let secondManagersQueue = ResultSet.findByName "secondManagersQueue"
  let firstHRServer = ResultSet.findByName "hrServer"
  let firstManagersServer = ResultSet.findByName "managersServer"
  let timer = ResultSet.findByName "timer"
  let successTimer = ResultSet.findByName "successTimer"
  let failureTimer = ResultSet.findByName "failureTimer"
  
  let providers = [
    ExperimentProvider.infiniteQueue firstHRQueue
    ExperimentProvider.infiniteQueue firstManagersQueue
    ExperimentProvider.infiniteQueue secondManagersQueue
    
    serverProvider firstHRServer
    serverProvider firstManagersServer
    
    ExperimentProvider.arrivalTimer timer
    ExperimentProvider.arrivalTimer successTimer
    ExperimentProvider.arrivalTimer failureTimer
  ]
  
  experiment.RenderHtml(model, providers)
  |> Async.RunSynchronously