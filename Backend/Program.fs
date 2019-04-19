module Fundshare.Main

open System
open System.Collections.Generic
open System.Net
open Suave
open Suave.Cookie
open Suave.Filters
open Suave.Operators
open Suave.Writers
open Newtonsoft.Json
open Chiron
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
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
  | Array arr -> (List.map chironJsonToObj arr) :> obj
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

type GraphQLServerReturn = { data : obj; errors: (NameValueLookup list) option }

[<EntryPoint>]
let main argv =
  if AppConfig.Boot.updateBalanceForAllUsers then
    do Repo.updateBalanceForAllUsers () |> ignore
    printfn "Balances recalculated and updated in database."

  let settings = JsonSerializerSettings()
  settings.Converters <- [| OptionConverter() :> JsonConverter |]
  settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
  let json o = JsonConvert.SerializeObject(o, settings)
  
  let schema = Schema(Query, Mutation, config = { SchemaConfig.Default with Types = AllGraphqlTypes })

  let authorize : OperationExecutionMiddleware = fun executionContext continueFn ->
    continueFn executionContext

  let executor = Executor(schema, [ ExecutorMiddleware(execute = authorize) ])

  let introspectionResult = executor.AsyncExecute(Introspection.introspectionQuery) |> Async.RunSynchronously
  let schemaStr : string =
    match introspectionResult with
      | Direct (data, errors) -> json data.["data"]
      | _ -> ""
  System.IO.File.WriteAllText(System.IO.Path.GetFullPath(AppConfig.Boot.outputSchemaPath), schemaStr)
  printfn "GraphQL schema was generated: %s" AppConfig.Boot.outputSchemaPath


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

        if AppConfig.General.debugLogging then do
          if authorizedUserId.IsSome then do printfn "%d" authorizedUserId.Value

        let session : Session ref =
          { authorizedUserId = authorizedUserId
            token = (if authorizedUserId.IsSome then token else None) }
          |> ref

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
                |> Option.map (fun query ->
                  let str = query.ToString().Trim().Replace("{", " {").Replace("\r\n", ", ").Replace("\n", " ").Trim()

                  // a hack for the frontend code that can't name things and backend graphql lib which can't have unnamed things... omg
                  if str.StartsWith("mutation  {") then
                    str.Insert("mutation".Length, " SomeMutation ")
                  else if str.StartsWith("query  {") then
                    str.Insert("query".Length, " SomeQuery ")
                  else
                    str
                )

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

        let unsetInvalidCookie session =
          if token.IsSome && (!session).token.IsNone then
            Cookie.unsetCookie AppConfig.Auth.cookieAuthName
          else
            WebPart.succeed

        let setAuthCookie session =
          if (!session).token.IsSome then
            let cookie =
              { (HttpCookie.createKV AppConfig.Auth.cookieAuthName (!session).token.Value)
                with expires =
                  Some <| DateTimeOffset.Now.AddMinutes(AppConfig.Auth.cookieMinutes |> float) }
            cookie |> Cookie.setCookie
          else
            WebPart.succeed

        let result : Result<GQLResponse, string> =
          try
            match query, variables with
            | Some query, Some variables ->
                if AppConfig.General.debugLogging then do
                  printfn "Received query: %s" query
                  printfn "Received variables: %A" variables

                executor.AsyncExecute(query, variables = variables, data = session)
            | Some query, _ ->
                if AppConfig.General.debugLogging then do
                  printfn "Received query: %s" query

                executor.AsyncExecute(query, data = session)
            | None, _ ->
                executor.AsyncExecute(Introspection.introspectionQuery)
            |> Async.RunSynchronously
            |> Result.Ok
          with
            | :? Exception as ex -> Result.Error ex.Message
            | _ -> Result.Error "internal error occured"

        return! http
        |> match result with
            | Result.Ok (Direct (data, errors)) ->
              let resultJson = json {
                data = data.["data"]
                errors =
                  if data.ContainsKey("errors")
                  then Some (data.["errors"] :?> NameValueLookup list)
                  else None
              }

              if AppConfig.General.debugLogging then do
                do printfn "Respond with: %s" resultJson |> ignore

              unsetInvalidCookie session >=> Successful.OK resultJson >=> setAuthCookie session
            | Result.Ok response ->
              let resultJson = json response.Content
              if AppConfig.General.debugLogging then do
                do printfn "Respond with: %s" resultJson |> ignore

              unsetInvalidCookie session >=> Successful.OK resultJson >=> setAuthCookie session
            | Result.Error err ->
              do printfn "Error: %s" err
              Successful.OK err
      }
  
  let setCorsHeaders = 
    setHeader  "Access-Control-Allow-Origin" "*"
    >=> setHeader "Access-Control-Allow-Headers" "content-type"
    
  let noCache = 
    setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
    >=> setHeader "Pragma" "no-cache"
    >=> setHeader "Expires" "0"
  
  let app : WebPart =
    choose [
      choose [Filters.GET; Filters.HEAD] >=> choose [
        path "/" >=> Files.browseFileHome "index.html"
        Files.browseHome
      ]
      path "/api" >=> setCorsHeaders >=> graphql >=> setMimeType "application/json"
      RequestErrors.NOT_FOUND "Page not found." 
    ] >=> noCache

  let httpFilesPath = System.IO.Path.GetFullPath(AppConfig.General.httpFilesPath)
  Console.WriteLine("Serving files from: " + httpFilesPath)

  let config = { defaultConfig with
    homeFolder = Some <| httpFilesPath
    hideHeader = true
    bindings = [ HttpBinding.create HTTP (IPAddress.Parse(AppConfig.General.listenHost)) (uint16 AppConfig.General.listenPort) ]
  }
  startWebServer config app
  
  0