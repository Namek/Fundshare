module Fundshare.Main

open System
open System.Net
open Suave
open Suave.Cookie
open Suave.Filters
open Suave.Operators
open Newtonsoft.Json
open Chiron
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Execution
open Fundshare.Api


let tryParse fieldName (data : byte array) =
  let raw = Text.Encoding.UTF8.GetString(data)
  try
    if raw <> null && raw <> "" then
      let map = JsonConvert.DeserializeObject<Map<string, obj>>(raw)
      Map.tryFind fieldName map
      |> Option.bind (fun value -> if value <> null then Some value else None)
    else None
  with _ ->
    None

let rec chironJsonToObj (value : Chiron.Json) : obj =
  match value with
  | String str -> str :> obj
  | Number n -> n :> obj
  | Bool b -> b :> obj
  | Null () -> null
  | Array omg -> (List.map chironJsonToObj omg) :> obj
  | Object map -> chironJsonMapToObjMap map :> obj

and chironJsonMapToObjMap (json : Map<string, Chiron.Json>) : Map<string, obj> =
  json |> Map.map (fun key value -> chironJsonToObj value)
  
type OptionConverter() =
  inherit JsonConverter()  
  override x.CanConvert(t) = 
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
  override x.WriteJson(writer, value, serializer) =
    let value = 
      if value = null then null
      else 
        let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
        fields.[0]  
    serializer.Serialize(writer, value)
  override x.ReadJson(reader, t, existingValue, serializer) = failwith "Not supported"  

type GraphQLServerReturn = { data : obj; errors: Error list }

[<EntryPoint>]
let main argv =
  let settings = JsonSerializerSettings()
  settings.Converters <- [| OptionConverter() :> JsonConverter |]
  settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
  let json o = JsonConvert.SerializeObject(o, settings)
  
  let schema = Schema(Query, Mutation, config = { SchemaConfig.Default with Types = AllGraphqlTypes })
  let executor = Executor(schema)
  

  let graphql : WebPart =
    fun http ->
      async {
        let token : string option =
          http.request.cookies.TryFind AppConfig.Auth.cookieAuthName
          |> Option.map (fun t -> t.value)

        let authorizedUserId = 
          token
          |> Option.bind parseToken
          |> Option.map (fun token ->
            match Int32.TryParse(token.Subject) with
            | (true, int) -> Some(int)
            | _ -> None)
          |> Option.defaultValue None

        if authorizedUserId.IsSome then do printfn "%d" authorizedUserId.Value

        let session : Session =
          { authorizedUserId = authorizedUserId
            token = if authorizedUserId.IsSome then token else None }

        let body = http.request.rawForm 
        let raw = Text.Encoding.UTF8.GetString(body)
        let (query, variables) = 
          try
            if raw <> null && raw <> "" then
              let map =
                match Chiron.Parsing.Json.tryParse raw with
                | Choice1Of2 (Object map) -> Some <| chironJsonMapToObjMap map
                | Choice2Of2 err -> None // TODO print err
                | _ -> None

              let query =
                Option.map (Map.tryFind "query") map
                |> Option.flatten
                |> Option.map (fun value -> value.ToString())
                |> Option.map (fun query -> query.ToString().Trim().Replace("{", " {").Replace("\r\n", ", ").Replace("\n", " "))

              let variables =
                Option.map (Map.tryFind "variables") map
                |> Option.flatten
                |> Option.bind (fun v -> if v <> null then Some v else None)
                |> Option.map (fun map -> map :?> Map<string, obj>)
              
              (query, variables)
            else
             (None, None)
          with _ ->
            (None, None)

        let setAuthCookie =
          if token.IsSome && session.token.IsNone then
              Cookie.unsetCookie AppConfig.Auth.cookieAuthName
          else if session.token.IsSome then
              Cookie.setCookie <| HttpCookie.createKV AppConfig.Auth.cookieAuthName session.token.Value
          else
            WebPart.succeed

        let result : Result<GQLResponse, string> =
          try
            match query, variables with
            | Some query, Some variables ->
                printfn "Received query: %s" query
                printfn "Received variables: %A" variables
                executor.AsyncExecute(query, variables = variables, data = session)
            | Some query, _ ->
                printfn "Received query: %s" query
                executor.AsyncExecute(query, data = session)
            | None, _ ->
                executor.AsyncExecute(Introspection.introspectionQuery)
            |> Async.RunSynchronously
            |> Result.Ok
          with
            | :? Exception as ex -> Result.Error ex.Message
            | _ -> Result.Error "internal error occured"


        return! http |>
          match result with
            | Result.Ok (Direct (data, errors)) ->
              setAuthCookie >=> Successful.OK (json {
                data = data.["data"]
                errors = if data.ContainsKey("errors") then data.["errors"] :?> Error list else []
              })
            | Result.Ok response ->
              setAuthCookie >=> Successful.OK (json response.Content)
            | Result.Error str -> Successful.OK str
      }
  
  let setCorsHeaders = 
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"
  
  let app : WebPart =
    choose [
      Filters.GET >=> path "/" >=> Files.browseFileHome "index.html"
      path "/api" >=> setCorsHeaders >=> graphql >=> Writers.setMimeType "application/json"
      Filters.GET >=> Files.browseHome  
      RequestErrors.NOT_FOUND "Page not found." 
    ]

  Console.WriteLine("Serving files from: " + AppConfig.General.httpFilesPath)
  
  let config = { defaultConfig with
    homeFolder = Some AppConfig.General.httpFilesPath
    hideHeader = true
    bindings = [ HttpBinding.create HTTP (IPAddress.Parse(AppConfig.General.listenHost)) (uint16 AppConfig.General.listenPort) ]
  }
  startWebServer config app
  
  0