module Common.Api.Models.UrlResponse exposing
    ( UrlResponse
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias UrlResponse =
    { contentType : String
    , url : String
    }


decoder : Decoder UrlResponse
decoder =
    D.succeed UrlResponse
        |> D.required "contentType" D.string
        |> D.required "url" D.string
