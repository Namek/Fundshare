-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.ScalarCodecs exposing (Date, Id, Uri, codecs)

import Api.Scalar exposing (defaultCodecs)
import Json.Decode as Decode exposing (Decoder)


type alias Date =
    Api.Scalar.Date


type alias Id =
    Api.Scalar.Id


type alias Uri =
    Api.Scalar.Uri


codecs : Api.Scalar.Codecs Date Id Uri
codecs =
    Api.Scalar.defineCodecs
        { codecDate = defaultCodecs.codecDate
        , codecId = defaultCodecs.codecId
        , codecUri = defaultCodecs.codecUri
        }