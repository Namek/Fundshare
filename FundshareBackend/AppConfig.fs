[<AutoOpen>]
module Fundshare.AppConfig

open FSharp.Configuration

type Config = YamlConfig<"Config.yaml">
let AppConfig = Config()
