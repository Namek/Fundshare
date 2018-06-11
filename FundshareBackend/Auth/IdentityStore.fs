module Fundshare.Auth.IdentityStore

open System.Security.Claims
open Fundshare.Repo

let getClaims (userId : int) =
    seq {
        yield (ClaimTypes.Name, userId)
    } |> Seq.map (fun x -> new Claim(fst x, (snd x).ToString())) |> async.Return
