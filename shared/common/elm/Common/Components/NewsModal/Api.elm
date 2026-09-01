module Common.Components.NewsModal.Api exposing (getNews)

import Common.Api.Models.RolePermission as RolePermission exposing (RolePermission)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Components.NewsModal.Models.New as New exposing (New)
import Json.Decode as D


getNews : String -> String -> List RolePermission -> ToMsg (List New) msg -> Cmd msg
getNews newsUrl version permissions =
    let
        serverInfo =
            { apiUrl = newsUrl
            , token = Nothing
            }

        permissionsParam =
            String.join "," (List.map RolePermission.toString permissions)
    in
    Request.get serverInfo ("?version=" ++ version ++ "&permissions=" ++ permissionsParam) (D.list New.decoder)
