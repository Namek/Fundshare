module Fundshare.Auth.Auth

open System
open Fundshare.Utils.Encodings
open Fundshare.Utils.SuaveJson
open JwtToken
open Suave.RequestErrors
open Suave
open Suave.Filters
open Suave.Http
open Suave.Operators


type TokenCreateCredentials = {
  UserName : string
  Password : string
  ClientId : string
}

type AuthConfig = {
  Audience : string
  Secret : Base64String
  Issuer : string
  TokenTimeSpan : TimeSpan
}

let authCreateToken config identityStore =
  let tryCreateToken (ctx: HttpContext) =
    match mapJsonPayload<TokenCreateCredentials> ctx.request with
    | None -> BAD_REQUEST "Invalid Token Create Request" ctx
    | Some tokenCreateCredentials ->
      async {
        let tokenCreateRequest : TokenCreateRequest = {
          Issuer = config.Issuer
          UserName = tokenCreateCredentials.UserName
          Password = tokenCreateCredentials.Password
          TokenTimeSpan = config.TokenTimeSpan
        }            
        let! token = createToken tokenCreateRequest identityStore config.Secret config.Audience
        match token with
        | Some token -> return! JSON token ctx
        | None -> return! BAD_REQUEST "Invalid Login Credentials" ctx
      }        
  
  tryCreateToken