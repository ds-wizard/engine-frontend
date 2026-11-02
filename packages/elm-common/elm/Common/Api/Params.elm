module Common.Api.Params exposing
    ( RequestParams
    , apply
    , none
    , withParams
    )

{-| Query parameters an application adds to requests of its own accord.

The API layer does not decide them. It tells the application which request it
is about to make — the method and the path — and the application answers with
the parameters to add, if any (`appState.requestParams`). What they are and why
is the application's business.

    Request.get serverInfo (Params.apply appState "GET" ("/projects" ++ queryString)) decoder

-}

import Url


type alias RequestParams =
    { method : String, path : String } -> List ( String, String )


{-| An application that adds nothing.
-}
none : RequestParams
none _ =
    []


{-| The URL with the application's parameters for this request added. The path
handed to the application is the URL without its query.
-}
apply : { a | requestParams : RequestParams } -> String -> String -> String
apply context method url =
    let
        path =
            String.split "?" url |> List.head |> Maybe.withDefault url
    in
    withParams (context.requestParams { method = method, path = path }) url


{-| Append query parameters to a URL that may already have some.
-}
withParams : List ( String, String ) -> String -> String
withParams params url =
    if List.isEmpty params then
        url

    else
        let
            separator =
                if String.contains "?" url then
                    "&"

                else
                    "?"
        in
        url ++ separator ++ String.join "&" (List.map (\( key, value ) -> key ++ "=" ++ Url.percentEncode value) params)
