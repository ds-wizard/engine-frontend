module Wizard.Data.Session exposing
    ( Session
    , decoder
    , encode
    , exists
    , expirationWarningMins
    , expired
    , expiresSoon
    , init
    , setFullscreen
    , setRightPanelCollapsed
    , setSidebarCollapsed
    , setToken
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Token as Token exposing (Token)
import Time


type alias Session =
    { token : Token
    , sidebarCollapsed : Bool
    , rightPanelCollapsed : Bool
    , fullscreen : Bool
    , apiUrl : String
    , v9 : Bool
    }


init : String -> Session
init apiUrl =
    { token = Token.empty
    , sidebarCollapsed = False
    , rightPanelCollapsed = True
    , fullscreen = False
    , apiUrl = apiUrl
    , v9 = True
    }


setToken : Session -> Token -> Session
setToken session token =
    { session | token = token }


setSidebarCollapsed : Session -> Bool -> Session
setSidebarCollapsed session collapsed =
    { session | sidebarCollapsed = collapsed }


setRightPanelCollapsed : Session -> Bool -> Session
setRightPanelCollapsed session collapsed =
    { session | rightPanelCollapsed = collapsed }


setFullscreen : Session -> Bool -> Session
setFullscreen session fullscreen =
    { session | fullscreen = fullscreen }


decoder : Decoder Session
decoder =
    D.succeed Session
        |> D.required "token" Token.decoder
        |> D.optional "sidebarCollapsed" D.bool False
        |> D.optional "rightPanelCollapsed" D.bool True
        |> D.optional "fullscreen" D.bool False
        |> D.required "apiUrl" D.string
        |> D.required "v9" D.bool


encode : Session -> E.Value
encode session =
    E.object
        [ ( "token", Token.encode session.token )
        , ( "sidebarCollapsed", E.bool session.sidebarCollapsed )
        , ( "rightPanelCollapsed", E.bool session.rightPanelCollapsed )
        , ( "fullscreen", E.bool session.fullscreen )
        , ( "apiUrl", E.string session.apiUrl )
        , ( "v9", E.bool session.v9 )
        ]


exists : Session -> Bool
exists session =
    session.token.token /= ""


expiresSoon : Time.Posix -> Session -> Bool
expiresSoon currentTimePosix session =
    let
        expiration =
            Time.posixToMillis session.token.expiresAt

        currentTime =
            Time.posixToMillis currentTimePosix
    in
    expiration - currentTime < expiresSoonTimeMillis


expired : Time.Posix -> Session -> Bool
expired currentTimePosix session =
    let
        expiration =
            Time.posixToMillis session.token.expiresAt

        currentTime =
            Time.posixToMillis currentTimePosix
    in
    expiration < currentTime


expiresSoonTimeMillis : Int
expiresSoonTimeMillis =
    expirationWarningMins * 60 * 1000


expirationWarningMins : Int
expirationWarningMins =
    10
