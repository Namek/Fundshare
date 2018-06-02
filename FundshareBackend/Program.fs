module Fundshare.Main

open System
open System.Threading
open System.Net
open System.Text
open System.IO

open Suave
open Suave.Filters
open Suave.Operators

open FSharp.Data.GraphQL
open Fundshare.DataStructures
open Fundshare.Api

let httpFilesPath = @"C:\Users\Namek\RiderProjects\Fundshare\FundshareFrontend\dist"


[<EntryPoint>]
let main argv =

//  getAllUsers() |> List.length |> Console.WriteLine
  
  let app : WebPart =
    choose [
      GET >=> path "/" >=> Files.browseFileHome "index.html"
      GET >=> Files.browseHome
      RequestErrors.NOT_FOUND "Page not found." 
    ]
  
  let config = { defaultConfig with homeFolder = Some httpFilesPath; hideHeader = true }
  startWebServer config app
  
  
  0