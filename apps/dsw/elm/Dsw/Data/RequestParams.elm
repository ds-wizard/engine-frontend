module Dsw.Data.RequestParams exposing (requestParams)

{-| What the client adds to its requests (`Common.Api.Params`).

Its tenant has one workspace, which the server assigns and the client never
names. The server only has to be told that roles, settings, and the knowledge
models, templates and secrets it creates or imports, are the tenant's:
`tenant=true`.

-}

import Common.Api.Params exposing (RequestParams)


requestParams : RequestParams
requestParams request =
    let
        isPull prefix =
            String.startsWith prefix request.path && String.endsWith "/pull" request.path

        isSettings =
            String.startsWith "/settings/" request.path
                || (request.path == "/plugin-settings")
                || String.startsWith "/plugin-settings/" request.path

        tenantRequest =
            case request.method of
                "GET" ->
                    request.path == "/roles" || isSettings

                "PUT" ->
                    isSettings

                "DELETE" ->
                    isSettings

                "POST" ->
                    List.member request.path [ "/roles", "/knowledge-model-secrets", "/knowledge-model-packages/bundle", "/document-templates/bundle" ]
                        || isPull "/knowledge-model-packages/"
                        || isPull "/document-templates/"

                _ ->
                    False
    in
    if tenantRequest then
        [ ( "tenant", "true" ) ]

    else
        []
