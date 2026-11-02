module Common.Api.Context exposing
    ( Context
    , toServerInfo
    )

{-| What a request needs to know about the application making it.

Every client has its own `AppState`; the API layer takes the fields it reads
instead, as an extensible record, so any client's state can be passed straight
in.

-}

import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Params exposing (RequestParams)
import Common.Api.Request exposing (ServerInfo)
import Common.Data.Session exposing (Session)
import Gettext
import String.Extra as String


type alias Context a =
    { a
        | apiUrl : String
        , session : Session
        , locale : Gettext.Locale
        , config : BootstrapConfig
        , urlCheckerUrl : Maybe String
        , requestParams : RequestParams
    }


toServerInfo : Context a -> ServerInfo
toServerInfo context =
    { apiUrl = context.apiUrl
    , token = String.toMaybe context.session.token.token
    }
