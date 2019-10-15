module Simulation.Aivika.Examples.Program

open ITMO.Modeling.FSharp
open ITMO.Modeling.FSharp
open ITMO.Modeling.FSharp
open ITMO.Modeling.FSharp
open Simulation.Aivika
open Simulation.Aivika
open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Charting.Web

[<EntryPoint>]
let main _ =
  let experiment = Experiment()

  experiment.Specs <- SimpleModel.specs
  experiment.RunCount <- 100

  let firstQueue = ResultSet.findByName "queue 1"
  let secondQueue = ResultSet.findByName "queue 2"
  let thirdQueue = ResultSet.findByName "queue 3"

  let firstServer = ResultSet.findByName "server 1"
  let secondServer = ResultSet.findByName "server 2"
  let thirdServer = ResultSet.findByName "server 3"
    
  let arrivalTimer = ResultSet.findByName "arrivalTimer"

  let providers = [
    ExperimentProvider.experimentSpecs
    
    ExperimentProvider.infiniteQueue firstQueue
    ExperimentProvider.queue secondQueue
    ExperimentProvider.queue thirdQueue
    
    ExperimentProvider.server firstServer
    ExperimentProvider.server secondServer
    ExperimentProvider.server thirdServer
    
    ExperimentProvider.arrivalTimer arrivalTimer
  ]

  experiment.RenderHtml(SimpleModel.createModel Coefficients.personCoefficients, providers)
  |> Async.RunSynchronously

  printfn "%A" Coefficients.personCoefficients
  0
