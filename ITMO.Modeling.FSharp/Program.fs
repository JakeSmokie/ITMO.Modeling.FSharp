﻿module Simulation.Aivika.Examples.Program

open ITMO.Modeling.FSharp

open ITMO.Modeling.FSharp.Coefficients
open System
open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Charting.Web

let chartWithStats rs =
  [
    ExperimentProvider.deviationChart rs
    ExperimentProvider.lastValueStats rs
  ] |> ExperimentProvider.concat

[<EntryPoint>]
let main _ =
  let experiment = Experiment()

  experiment.Specs <- SimpleModel.specs
  experiment.RunCount <- 1

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
  let arrivalTimerVC = ResultSet.findByName "arrivalTimerVC"

  let firstServerLoad = ResultSet.findByName "server 1 load"
  let secondServerLoad = ResultSet.findByName "server 2 load"
  let thirdServerLoad = ResultSet.findByName "server 3 load"

  let providers = [
    ExperimentProvider.experimentSpecs

    ExperimentProvider.infiniteQueue firstQueue
    ExperimentProvider.queue secondQueue
    ExperimentProvider.queue thirdQueue

    chartWithStats secondQueueLossProb
    chartWithStats thirdQueueLossProb

    ExperimentProvider.server firstServer
    ExperimentProvider.server secondServer
    ExperimentProvider.server thirdServer

    chartWithStats firstServerLoad
    chartWithStats secondServerLoad
    chartWithStats thirdServerLoad

    chartWithStats firstServerTime
    chartWithStats secondServerTime
    chartWithStats thirdServerTime

    chartWithStats firstSectionTime
    chartWithStats secondSectionTime
    chartWithStats thirdSectionTime

    ExperimentProvider.arrivalTimer arrivalTimer
    chartWithStats arrivalTimerVC
  ]

  let coefficients =
    Coefficients.personCoefficients

  let all =
    [
      coefficients
//      {coefficients with ChannelsCount = coefficients.ChannelsCount * 2}
//      {coefficients with ChannelsCount = 1}
    ] |> List.collect (fun c ->
      [
//       {c with Distributions = BothExponential}
//       {c with Distributions = ConstAndUniform}
       {c with Distributions = ErlangAndUniform}
//       {c with Distributions = ErlangAndHyper}
      ]
    )

  List.iter (printfn "%A") all

  List.iter (fun coefficients ->
    experiment.RenderHtml(SimpleModel.createModel coefficients, providers)
    |> Async.RunSynchronously
  ) all

  0
