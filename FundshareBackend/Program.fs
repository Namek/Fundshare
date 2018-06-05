module Fundshare.Main

open System
open System.Threading
open System.Net
open System.Text
open System.IO

open Suave
open Suave.Filters
open Suave.Operators
open Newtonsoft.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

open Fundshare.DataStructures
open Fundshare.Api

let httpFilesPath = @"C:\Users\Namek\RiderProjects\Fundshare\FundshareFrontend\dist"

let tryParse fieldName (data : byte array) =
  let raw = Text.Encoding.UTF8.GetString(data)
  if raw <> null && raw <> ""
  then
    let map = JsonConvert.DeserializeObject<Map<string,string>>(raw)
    Map.tryFind fieldName map
  else None
  
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

type GraphQLServerReturn = { data : Output; errors: Error list }

[<EntryPoint>]
let main argv =

  let settings = JsonSerializerSettings()
  settings.Converters <- [| OptionConverter() :> JsonConverter |]
  settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
  let json o = JsonConvert.SerializeObject(o, settings)
  
  let schema = Schema(Query, Mutation, config = { SchemaConfig.Default with Types = AllGraphqlTypes })
  let executor = Executor(schema)
  
  let executeQuery (query : string) =
      async {
        let! result = executor.AsyncExecute(query)
        match result with
        | Direct (data, errors) ->
          return Successful.OK (json {data = data; errors = errors})
        | _ ->
          return Successful.OK (json result.Content)
      }

  let graphiql : WebPart =
    fun http ->
      async {
        let q = match tryParse "query" http.request.rawForm with
          | Some query ->
            // at the moment parser is not parsing new lines correctly, so we need to get rid of them
            query.Trim().Replace("\r\n", " ").Replace("\n", " ")
              
          | None ->
            Introspection.introspectionQuery
        
        let! res = executeQuery q
        return! http |> res
      }
  
  let setCorsHeaders = 
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"
  
  let app : WebPart =
    choose [
      GET >=> path "/" >=> Files.browseFileHome "index.html"
      GET >=> Files.browseHome
      path "/api" >=> setCorsHeaders >=> graphiql >=> Writers.setMimeType "application/json"
      RequestErrors.NOT_FOUND "Page not found." 
    ]
  
  let config = { defaultConfig with homeFolder = Some httpFilesPath; hideHeader = true }
  startWebServer config app
  
  
  0