// in-memory symmetric security key based on HMAC
module Fundshare.Auth.KeyStore

open Fundshare.Utils.Encodings
open System.Security.Cryptography
open Microsoft.IdentityModel.Tokens


let securityKey sharedKey : SecurityKey =
  let symmetricKey = sharedKey |> Base64String.decode
  new SymmetricSecurityKey(symmetricKey) :> SecurityKey

let hmacSha256 secretKey =
  new SigningCredentials(secretKey,
    SecurityAlgorithms.HmacSha256Signature, 
    SecurityAlgorithms.Sha256Digest)

let createSecret =
    let data = Array.zeroCreate 32
    RNGCryptoServiceProvider.Create().GetBytes(data)
    Base64String.create data