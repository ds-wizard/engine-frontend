module Shared.Auth.Session exposing
    ( Session
    , decoder
    , encode
    , exists
    , getUserRole
    , getUserUuid
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
import Maybe.Extra as Maybe
import Shared.Data.Token as Token exposing (Token)
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)
import Uuid


type alias Session =
    { token : Token
    , user : Maybe UserInfo
    , sidebarCollapsed : Bool
    , fullscreen : Bool
    , v6 : Bool
    }


init : Session
init =
    { token = Token.empty
    , user = Nothing
    , sidebarCollapsed = False
    , fullscreen = False
    , v6 = True
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


getUserUuid : Session -> Maybe String
getUserUuid session =
    Maybe.map (Uuid.toString << .uuid) session.user


getUserRole : Session -> String
getUserRole =
    Maybe.unwrap "" .role << .user


decoder : Decoder Session
decoder =
    D.succeed Session
        |> D.required "token" Token.decoder
        |> D.required "user" (D.nullable UserInfo.decoder)
        |> D.optional "sidebarCollapsed" D.bool False
        |> D.optional "fullscreen" D.bool False
        |> D.required "v6" D.bool


encode : Session -> E.Value
encode session =
    E.object
        [ ( "token", Token.encode session.token )
        , ( "user", E.maybe UserInfo.encode session.user )
        , ( "sidebarCollapsed", E.bool session.sidebarCollapsed )
        , ( "fullscreen", E.bool session.fullscreen )
        , ( "v6", E.bool session.v6 )
        ]


exists : Session -> Bool
exists session =
    session.token.token /= ""
