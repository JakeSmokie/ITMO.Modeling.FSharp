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
  experiment.RunCount <- 100

  let firstQueue = ResultSet.findByName "queue 1"
  let secondQueue = ResultSet.findByName "queue 2"
  let thirdQueue = ResultSet.findByName "queue 3"

  let secondQueueLossProb = ResultSet.findByName "queue 2 loss prob"
  let thirdQueueLossProb = ResultSet.findByName "queue 3 loss prob"

  let firstServer = ResultSet.findByName "server 1"
  let secondServer = ResultSet.findByName "server 2"
  let thirdServer = ResultSet.findByName "server 3"

  let firstServerTime = ResultSet.findByName "server 1 time"
  let secondServerTime = ResultSet.findByName "server 2 time"
  let thirdServerTime = ResultSet.findByName "server 3 time"

  let firstSectionTime = ResultSet.findByName "queue + server 1 time"
  let secondSectionTime = ResultSet.findByName "queue + server 2 time"
  let thirdSectionTime = ResultSet.findByName "queue + server 3 time"

  let arrivalTimer = ResultSet.findByName "arrivalTimer"

  let providers = [
    ExperimentProvider.experimentSpecs

    ExperimentProvider.infiniteQueue firstQueue
    ExperimentProvider.queue secondQueue
    ExperimentProvider.queue thirdQueue

    ExperimentProvider.deviationChart secondQueueLossProb
    ExperimentProvider.deviationChart thirdQueueLossProb

    ExperimentProvider.lastValueStats secondQueueLossProb
    ExperimentProvider.lastValueStats thirdQueueLossProb

    ExperimentProvider.server firstServer
    ExperimentProvider.server secondServer
    ExperimentProvider.server thirdServer

    ExperimentProvider.deviationChart firstServerTime
    ExperimentProvider.deviationChart secondServerTime
    ExperimentProvider.deviationChart thirdServerTime

    ExperimentProvider.lastValueStats firstServerTime
    ExperimentProvider.lastValueStats secondServerTime
    ExperimentProvider.lastValueStats thirdServerTime

    ExperimentProvider.deviationChart firstSectionTime
    ExperimentProvider.deviationChart secondSectionTime
    ExperimentProvider.deviationChart thirdSectionTime

    ExperimentProvider.lastValueStats firstSectionTime
    ExperimentProvider.lastValueStats secondSectionTime
    ExperimentProvider.lastValueStats thirdSectionTime

    ExperimentProvider.arrivalTimer arrivalTimer
  ]

  experiment.RenderHtml(SimpleModel.createModel Coefficients.personCoefficients, providers)
  |> Async.RunSynchronously

  printfn "%A" Coefficients.personCoefficients
  0
