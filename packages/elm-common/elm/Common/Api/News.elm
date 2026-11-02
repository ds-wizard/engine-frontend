module Common.Api.News exposing (getNews)

{-| The news feed is not the application's API: it is a separate, public
service, so the request carries no token, and the version and permissions go in
the query string so the feed can choose what this user is shown.
-}

import Common.Api.Models.RolePermission as RolePermission exposing (RolePermission)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.News as News exposing (News)
import Json.Decode as D


getNews : String -> String -> List RolePermission -> ToMsg (List News) msg -> Cmd msg
getNews newsUrl version permissions =
    let
        serverInfo =
            { apiUrl = newsUrl
            , token = Nothing
            }

        permissionsParam =
            String.join "," (List.map RolePermission.toString permissions)
    in
    Request.get serverInfo ("?version=" ++ version ++ "&permissions=" ++ permissionsParam) (D.list News.decoder)
