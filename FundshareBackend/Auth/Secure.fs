module Fundshare.Auth.Secure

open Fundshare.DataStructures
open Suave.RequestErrors
open Microsoft.IdentityModel.Tokens
open Suave
open JwtToken
open System.Security.Claims

// Abstracts the underlying audience from the validation logic so that it can be reused across multiple audiences
type JwtConfig = {
    Issuer : string    
    SecurityKey : SecurityKey
    ClientId : string    
}

type AuthorizationResult =
  | Authorized of User
  | UnAuthorized of string

// Validates the access token present in the request header and invokes the given WebPart if it is valid. 
// In case of invalid or absence of access token, it returns an HTTP error response instead of executing the WebPart.
// Upon successful access token validation, it puts the claims in the userState map of incoming HttpContext 
// so that subsequent WebParts in the pipeline can use it.
let jwtAuthenticate jwtConfig webpart (ctx: HttpContext) =

  let updateContextWithClaims claims =
    { ctx with 
        userState = ctx.userState.Remove("Claims").Add("Claims", claims)}    

  // TODO "authorization: Bearer <token>"
  match ctx.request.header "authorization" with
  | Choice1Of2 accessToken ->
    let tokenValidationRequest =  {
      Issuer = jwtConfig.Issuer
      SecurityKey = jwtConfig.SecurityKey
      ClientId = jwtConfig.ClientId
      AccessToken = accessToken
    }
    let validationResult = validate tokenValidationRequest
    match validationResult with
    | Choice1Of2 claims -> webpart (updateContextWithClaims claims)
    | Choice2Of2 err -> FORBIDDEN err ctx

  | _ -> 
    BAD_REQUEST "Invalid Request. Provide both clientid and token" ctx



// Provides authorization in addition to the authentication.
let jwtAuthorize jwtConfig authorizeUser webpart  =
  let getClaims (ctx: HttpContext) =
    let userState = ctx.userState
    if userState.ContainsKey("Claims") then
      match userState.Item "Claims" with
      | :? (Claim seq) as claims -> Some claims
      | _ -> None
    else
        None

  let authorize httpContext =
    match getClaims httpContext with
    | Some claims ->
        async {
          let! authorizationResult = authorizeUser claims
          match authorizationResult with
          | Authorized user -> return! webpart httpContext
          | UnAuthorized err -> return! FORBIDDEN err httpContext
        }
    | None -> FORBIDDEN "Claims not found" httpContext

  jwtAuthenticate jwtConfig authorize