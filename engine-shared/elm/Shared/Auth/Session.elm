module Shared.Auth.Session exposing
    ( Session
    , decoder
    , encode
    , exists
    , init
    , setFullscreen
    , setSidebarCollapsed
    , setToken
    , setUser
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Token as Token exposing (Token)
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)


type alias Session =
    { token : Token
    , user : Maybe UserInfo
    , sidebarCollapsed : Bool
    , fullscreen : Bool
    , v5 : Bool
    }


init : Session
init =
    { token = Token.empty
    , user = Nothing
    , sidebarCollapsed = False
    , fullscreen = False
    , v5 = True
    }


setToken : Session -> Token -> Session
setToken session token =
    { session | token = token }


setUser : Session -> UserInfo -> Session
setUser session user =
    { session | user = Just user }


setSidebarCollapsed : Session -> Bool -> Session
setSidebarCollapsed session collapsed =
    { session | sidebarCollapsed = collapsed }


setFullscreen : Session -> Bool -> Session
setFullscreen session fullscreen =
    { session | fullscreen = fullscreen }


decoder : Decoder Session
decoder =
    D.succeed Session
        |> D.required "token" Token.decoder
        |> D.required "user" (D.nullable UserInfo.decoder)
        |> D.optional "sidebarCollapsed" D.bool False
        |> D.optional "fullscreen" D.bool False
        |> D.required "v5" D.bool


encode : Session -> E.Value
encode session =
    E.object
        [ ( "token", Token.encode session.token )
        , ( "user", E.maybe UserInfo.encode session.user )
        , ( "sidebarCollapsed", E.bool session.sidebarCollapsed )
        , ( "fullscreen", E.bool session.fullscreen )
        , ( "v5", E.bool session.v5 )
        ]


exists : Session -> Bool
exists session =
    session.token.token /= ""
