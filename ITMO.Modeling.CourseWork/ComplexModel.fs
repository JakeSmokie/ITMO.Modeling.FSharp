module ITMO.Modeling.CourseWork.ComplexModel

open Simulation.Aivika
open Simulation.Aivika.Charting.Gtk.Web
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Results
open ITMO.Modeling.CourseWork.Base

let cvPerDay = 4.
let streamDelay = days 1 / cvPerDay

let hrChoiceProbability = 0.7
let cvTransferServingTime = minutes 30

let managersAmount = 40
let hrAmount = 4

let successProbabilty = 0.9

let model = simulation {
  let! timer = ArrivalTimer.create
  let! successTimer = ArrivalTimer.create
  let! failureTimer = ArrivalTimer.create
  
  let! firstManagersQueue = queue
  let! firstHRQueue = queue 
  let! secondManagersQueue = queue
  let! secondHRQueue = queue
  let! firstInterviewQueue = queue
  let! secondInterviewQueue = queue
  let! failureQueue = queue
  let! successQueue = queue
  
  let! managers = createServer managersAmount ^ fun _ -> Server.createRandomExponential cvTransferServingTime
  let! recruiters = createServer hrAmount ^ fun _ -> Server.createRandomExponential cvTransferServingTime

  // Util
  let try_ success prob = choose success failureQueue prob

  // Here goes action
  // M or HR
  let stream = Stream.randomExponential streamDelay
  do!
    stream
    |> choose firstHRQueue firstManagersQueue hrChoiceProbability
    |> run
  
  let! delay = createServer 10000 ^ fun _ -> Server.createRandomExponential ^ days 1 
  
  // Manager -> HRs
  do!
    dequeue firstManagersQueue
    |> serve managers
    |> serve delay
    |> goto firstHRQueue
    |> run
  
  // HRs
  let scale = 7
  let mean = hours 3

  let! delay = createServer 10000 ^ fun _ -> Server.createRandomErlang (float scale / mean) scale  

  do!
    dequeue firstHRQueue
    |> serve recruiters
    |> serve delay
    |> try_ secondManagersQueue successProbabilty
    |> run
  
  let! delay = createServer 10000 ^ fun _ -> Server.createRandomExponential ^ days 1  

  // Managers
  do!
    dequeue secondManagersQueue
    |> serve managers
    |> serve delay
    |> try_ secondHRQueue successProbabilty
    |> run
    
  let! delay = createServer 10000 ^ fun _ -> Server.createRandomExponential ^ days 7
  
  do!
    dequeue secondHRQueue
    |> serve recruiters
    |> serve delay
    |> try_ firstInterviewQueue successProbabilty
    |> run
  
  let! delay = createServer 10000 ^ fun _ -> Server.createRandomExponential ^ days 1

  do!
    dequeue firstInterviewQueue
    |> serve recruiters
    |> serve managers
    |> serve delay
    |> Processor.arrc ^ fun x -> proc {
      let! success = Parameter.lift ^ Parameter.randomTrue successProbabilty
      let! secondInterview = Parameter.lift ^ Parameter.randomTrue successProbabilty

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
  
  let! delay = createServer 10000 ^ fun _ -> Server.createRandomExponential ^ days 2

  do!
    dequeue secondInterviewQueue
    |> serve recruiters
    |> serve managers
    |> serve delay
    |> try_ successQueue successProbabilty
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
    ResultSource.From("firstManagersQueue", firstManagersQueue)
    ResultSource.From("firstHRQueue", firstHRQueue)
    ResultSource.From("secondManagersQueue", secondManagersQueue)
    ResultSource.From("secondHRQueue", secondHRQueue)
    ResultSource.From("firstInterviewQueue", firstInterviewQueue)
    ResultSource.From("secondInterviewQueue", secondInterviewQueue)
    ResultSource.From("failureQueue", failureQueue)
    ResultSource.From("successQueue", successQueue)
    ResultSource.From("hrServer", recruiters)
    ResultSource.From("managersServer", managers)
    ResultSource.From("timer", timer)
    ResultSource.From("successTimer", successTimer)
    ResultSource.From("failureTimer", failureTimer)
  ] |> ResultSet.create
}

let specs = {
  StartTime = 0.0
  StopTime = days 30
  DT = minutes 1
  Method = RungeKutta4
  GeneratorType = StrongGenerator
}

let runCount = 1
let run() =
  let experiment = Experiment()

  experiment.Specs <- specs
  experiment.RunCount <- runCount

  let firstManagersQueue = ResultSet.findByName "firstManagersQueue"
  let firstHRQueue = ResultSet.findByName "firstHRQueue"
  let secondManagersQueue = ResultSet.findByName "secondManagersQueue"
  let secondHRQueue = ResultSet.findByName "secondHRQueue"
  let firstInterviewQueue = ResultSet.findByName "firstInterviewQueue"
  let secondInterviewQueue = ResultSet.findByName "secondInterviewQueue"
  let failureQueue = ResultSet.findByName "failureQueue"
  let successQueue = ResultSet.findByName "successQueue"
  
  let firstHRServer = ResultSet.findByName "hrServer"
  let firstManagersServer = ResultSet.findByName "managersServer"
  let timer = ResultSet.findByName "timer"
  let successTimer = ResultSet.findByName "successTimer"
  let failureTimer = ResultSet.findByName "failureTimer"
  
  let providers = [
    ExperimentProvider.arrivalTimer timer
    ExperimentProvider.arrivalTimer successTimer
    ExperimentProvider.arrivalTimer failureTimer
    
    ExperimentProvider.infiniteQueue firstManagersQueue
    ExperimentProvider.infiniteQueue firstHRQueue
    ExperimentProvider.infiniteQueue secondManagersQueue
    ExperimentProvider.infiniteQueue secondHRQueue
    ExperimentProvider.infiniteQueue firstInterviewQueue
    ExperimentProvider.infiniteQueue secondInterviewQueue
//    ExperimentProvider.infiniteQueue failureQueue
//    ExperimentProvider.infiniteQueue successQueue
    
    serverProvider firstHRServer
    serverProvider firstManagersServer    
  ]
  
  experiment.RenderHtml(model, providers)
  |> Async.RunSynchronously