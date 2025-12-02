module Wizard.Api.Models.UrlCheckRequest exposing
    ( UrlCheckRequest
    , encode
    )

import Json.Encode as E


type alias UrlCheckRequest =
    { urls : List String
    }


encode : UrlCheckRequest -> E.Value
encode request =
    E.object
        [ ( "urls", E.list E.string request.urls )
        ]
