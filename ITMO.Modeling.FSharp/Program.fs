module Simulation.Aivika.Examples.Program

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Charting.Web

[<EntryPoint>]
let main _ =
  let experiment = Experiment()

  experiment.Specs <- Model.specs
  experiment.RunCount <- 1000

  let queueSeries1 = ResultSet.findByName "queue1"
  let queueSeries2 = ResultSet.findByName "queue2"

  let serverSeries1 = ResultSet.findByName "workStation1"
  let serverSeries2 = ResultSet.findByName "workStation2"

  let timerSeries = ResultSet.findByName "arrivalTimer"

  let providers = [
    ExperimentProvider.experimentSpecs
    ExperimentProvider.queue queueSeries1
    ExperimentProvider.server serverSeries1
    ExperimentProvider.queue queueSeries2
    ExperimentProvider.server serverSeries2
    ExperimentProvider.arrivalTimer timerSeries
  ]

  experiment.RenderHtml(Model.model, providers)
  |> Async.RunSynchronously

  0
