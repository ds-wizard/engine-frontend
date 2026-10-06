module Common.Data.Session exposing
    ( Session
    , decoder
    , encode
    , exists
    , expirationWarningMins
    , expired
    , expiresSoon
    , init
    , isValid
    , setFullscreen
    , setRightPanelCollapsed
    , setSidebarCollapsed
    , setToken
    )

import Common.Api.Models.Token as Token exposing (Token)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time


type alias Session =
    { token : Token
    , sidebarCollapsed : Bool
    , rightPanelCollapsed : Bool
    , fullscreen : Bool
    , apiUrlBase : String
    , v10 : Bool
    }


{-| The API URL base is the part all the apps share, each of them appends its own `/<app>-api` to
it. It is kept in the session so that the next load knows where to bootstrap from.
-}
init : String -> Session
init apiUrlBase =
    { token = Token.empty
    , sidebarCollapsed = False
    , rightPanelCollapsed = True
    , fullscreen = False
    , apiUrlBase = apiUrlBase
    , v10 = True
    }


decoder : Decoder Session
decoder =
    D.succeed Session
        |> D.required "token" Token.decoder
        |> D.optional "sidebarCollapsed" D.bool False
        |> D.optional "rightPanelCollapsed" D.bool True
        |> D.optional "fullscreen" D.bool False
        |> D.optional "apiUrlBase" D.string ""
        |> D.required "v10" D.bool


encode : Session -> E.Value
encode session =
    E.object
        [ ( "token", Token.encode session.token )
        , ( "sidebarCollapsed", E.bool session.sidebarCollapsed )
        , ( "rightPanelCollapsed", E.bool session.rightPanelCollapsed )
        , ( "fullscreen", E.bool session.fullscreen )
        , ( "apiUrlBase", E.string session.apiUrlBase )
        , ( "v10", E.bool session.v10 )
        ]


exists : Session -> Bool
exists session =
    session.token.token /= ""


isValid : Time.Posix -> Session -> Bool
isValid currentTime session =
    exists session && not (expired currentTime session)


expired : Time.Posix -> Session -> Bool
expired currentTimePosix session =
    Time.posixToMillis session.token.expiresAt < Time.posixToMillis currentTimePosix


expiresSoon : Time.Posix -> Session -> Bool
expiresSoon currentTimePosix session =
    let
        expiration =
            Time.posixToMillis session.token.expiresAt

        currentTime =
            Time.posixToMillis currentTimePosix
    in
    expiration - currentTime < expiresSoonTimeMillis


expiresSoonTimeMillis : Int
expiresSoonTimeMillis =
    expirationWarningMins * 60 * 1000


expirationWarningMins : Int
expirationWarningMins =
    10


setToken : Token -> Session -> Session
setToken token session =
    { session | token = token }


setSidebarCollapsed : Bool -> Session -> Session
setSidebarCollapsed sidebarCollapsed session =
    { session | sidebarCollapsed = sidebarCollapsed }


setRightPanelCollapsed : Bool -> Session -> Session
setRightPanelCollapsed rightPanelCollapsed session =
    { session | rightPanelCollapsed = rightPanelCollapsed }


setFullscreen : Bool -> Session -> Session
setFullscreen fullscreen session =
    { session | fullscreen = fullscreen }
