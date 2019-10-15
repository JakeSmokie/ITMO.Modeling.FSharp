module ITMO.Modeling.FSharp.Coefficients

type Person = {
  Surname: string
  Name: string
}

let person = {
  Surname = "Айгузин"
  Name = "Иван"
}

type Coefficients = {
  ChannelsCount: int
  WorkTime: float
  StreamRate: float
  BranchProbability: float
  Capacity2: int
  Capacity3: int
}

let getCoefficientsForPerson person =
  let a = person.Surname.Length
  let b = person.Name.Length

  let channelsCount = 2 + b % 7
  
  {
    ChannelsCount = channelsCount
    WorkTime = float a
    StreamRate = (float channelsCount) * 0.9 / (float a)
    BranchProbability = (float a) / (float a + float b)
    Capacity2 = 3 + a % 5
    Capacity3 = 6 - a % 5
  }

let personCoefficients =
  getCoefficientsForPerson person