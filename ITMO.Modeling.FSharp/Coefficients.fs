module ITMO.Modeling.FSharp.Coefficients

type Person = {
  Surname: string
  Name: string
}

let person = {
  Surname = "Айгузин"
  Name = "Иван"
}

type Distributions =
  | BothExponential
  | ConstAndUniform
  | ErlangAndUniform
  | ErlandAndHyper

type Coefficients = {
  ChannelsCount: int
  WorkTime: float
  StreamDelay: float
  BranchProbability: float
  Capacity2: int
  Capacity3: int

  Distributions: Distributions
  VC: float
}

let getCoefficientsForPerson person =
  let a = person.Surname.Length
  let b = person.Name.Length

  let channelsCount = 2 + b % 7
  
  {
    ChannelsCount = channelsCount
    WorkTime = float a
    StreamDelay = (float a) / ((float channelsCount) * 0.9)
    BranchProbability = (float a) / (float a + float b)
    Capacity2 = 3 + a % 5
    Capacity3 = 6 - a % 5
  
    Distributions = BothExponential
    VC = 0.3
  }

let personCoefficients =
  getCoefficientsForPerson person