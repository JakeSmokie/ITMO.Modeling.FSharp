module Simulation.Aivika.Examples.Program

open ITMO.Modeling.FSharp
open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Charting.Web

[<EntryPoint>]
let main _ =
  let experiment = Experiment()

  experiment.Specs <- SimpleModel.specs
  experiment.RunCount <- 1

  let queue = ResultSet.findByName "queue"
  let workStation = ResultSet.findByName "workStation"
  let arrivalTimer = ResultSet.findByName "arrivalTimer"

  let providers = [
    ExperimentProvider.experimentSpecs
    ExperimentProvider.infiniteQueue queue
    ExperimentProvider.server workStation
    ExperimentProvider.arrivalTimer arrivalTimer
  ]

  experiment.RenderHtml(SimpleModel.model, providers)
  |> Async.RunSynchronously

  0
