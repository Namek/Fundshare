module Fundshare.Auth.JwtToken

open Fundshare.Utils.Encodings
open System
open System.Security.Claims
open System.Security.Cryptography
open Microsoft.IdentityModel.Tokens
open System.IdentityModel.Tokens.Jwt
open Fundshare.DataStructures


type TokenCreateRequest = {         
  Issuer : string        
  UserName : string
  Password : string        
  TokenTimeSpan : TimeSpan
}

type IdentityStore = {
  getClaims : int -> Async<Claim seq>
  validateUserCredentials : string -> string -> User option
  getSecurityKey : Base64String -> SecurityKey
  getSigningCredentials : SecurityKey -> SigningCredentials
}

type Token = {
  AccessToken : string
  ExpiresIn : float
}

let createToken request identityStore secret audience =
  async {
    let creds = identityStore.validateUserCredentials request.UserName request.Password

    match creds with
    | None -> return None
    | Some user ->
      let signingCredentials =
        secret
        |> (identityStore.getSecurityKey >> identityStore.getSigningCredentials)
      let issuedOn = Nullable DateTime.UtcNow
      let expiresBy = Nullable (DateTime.UtcNow.Add(request.TokenTimeSpan))       
      let! claims = identityStore.getClaims user.id
      let jwtSecurityToken = new JwtSecurityToken(request.Issuer, audience, claims, issuedOn, expiresBy, signingCredentials)
      let handler = new JwtSecurityTokenHandler()
      let accessToken = handler.WriteToken(jwtSecurityToken)                
      return Some {
        AccessToken = accessToken                    
        ExpiresIn = request.TokenTimeSpan.TotalSeconds
      }
  }

type TokenValidationRequest = {
  Issuer : string
  SecurityKey : SecurityKey
  ClientId : string
  AccessToken : string
}

let validate (validationRequest : TokenValidationRequest) =
  let tokenValidationParameters =
    let validationParams = new TokenValidationParameters()
    validationParams.ValidAudience <- validationRequest.ClientId
    validationParams.ValidIssuer <- validationRequest.Issuer
    validationParams.ValidateLifetime <- true
    validationParams.ValidateIssuerSigningKey <- true
    validationParams.IssuerSigningKey <-  validationRequest.SecurityKey
    validationParams
  try
    let handler = new JwtSecurityTokenHandler()
    let principal =
      handler.ValidateToken(validationRequest.AccessToken, 
                                tokenValidationParameters, ref null)
    principal.Claims |> Choice1Of2
  with
    | ex -> ex.Message |> Choice2Of2