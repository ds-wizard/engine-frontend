module Common.Components.NewsModal.Api exposing (getNews)

import Common.Api.Request as Request exposing (ToMsg)
import Common.Components.NewsModal.Models.New as New exposing (New)
import Json.Decode as D


getNews : String -> String -> ToMsg (List New) msg -> Cmd msg
getNews newsUrl version =
    let
        serverInfo =
            { apiUrl = newsUrl
            , token = Nothing
            }
    in
    Request.get serverInfo ("?version=" ++ version) (D.list New.decoder)
