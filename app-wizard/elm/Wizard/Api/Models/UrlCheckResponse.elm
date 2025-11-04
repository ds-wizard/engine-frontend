module Wizard.Api.Models.UrlCheckResponse exposing
    ( UrlCheckResponse
    , decoder
    , getResultByUrl
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Wizard.Api.Models.UrlCheckResponse.UrlResult as UrlResult exposing (UrlResult)


type alias UrlCheckResponse =
    { results : List UrlResult
    , okCount : Int
    , errorCount : Int
    }


decoder : Decoder UrlCheckResponse
decoder =
    D.succeed UrlCheckResponse
        |> D.required "results" (D.list UrlResult.decoder)
        |> D.required "ok_count" D.int
        |> D.required "error_count" D.int


getResultByUrl : String -> UrlCheckResponse -> Maybe UrlResult
getResultByUrl url response =
    List.find (\result -> result.url == url) response.results
