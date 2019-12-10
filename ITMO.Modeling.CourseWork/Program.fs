open System
open ITMO.Modeling.CourseWork
open ITMO.Modeling.CourseWork.Base

[<EntryPoint>]
let main argv =
  let time = DateTime.Now
  printfn "Time: %A" time

//  AnalyticModel.run()
  ComplexModel.run()

  printfn "Time: %A. %A" DateTime.Now ^ DateTime.Now - time

  0
