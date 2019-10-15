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

  let queue = ResultSet.findByName "queue"
  let workStation = ResultSet.findByName "workStation"
  let arrivalTimer = ResultSet.findByName "arrivalTimer"
  let avg = ResultSet.findByName "avg"

  let providers = [
    ExperimentProvider.experimentSpecs
    ExperimentProvider.infiniteQueue queue
    ExperimentProvider.server workStation
    ExperimentProvider.arrivalTimer arrivalTimer
    ExperimentProvider.lastValueHistogram avg
  ]

  experiment.RenderHtml(SimpleModel.createModel Coefficients.personCoefficients, providers)
  |> Async.RunSynchronously

  printfn "%A" Coefficients.personCoefficients
  0
