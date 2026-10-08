namespace Portfolio
open System.Text.Json
type Api =
    static member Run(action: string, input: string) =
        let options = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase, PropertyNameCaseInsensitive = true)
        match action with
        | "health" -> """{"engine":"F#","version":"1.0","model":"finite-horizon-queue-network"}"""
        | "simulate" ->
            let config = JsonSerializer.Deserialize<Config>(input, options)
            if obj.ReferenceEquals(config, null) then invalidArg "input" "Не переданы параметры."
            JsonSerializer.Serialize(Simulation.experiment config, options)
        | _ -> invalidArg "action" "Неизвестная операция."
