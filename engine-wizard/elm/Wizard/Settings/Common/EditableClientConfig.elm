module Wizard.Settings.Common.EditableClientConfig exposing
    ( EditableClientConfig
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Common.Config.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)
import Wizard.Common.Config.DashboardWidget as DashboardWidget exposing (DashboardWidget)


type alias EditableClientConfig =
    { appTitle : Maybe String
    , appTitleShort : Maybe String
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    , loginInfo : Maybe String
    , dashboard : Maybe (Dict String (List DashboardWidget))
    , privacyUrl : Maybe String
    , customMenuLinks : List CustomMenuLink
    , supportEmail : Maybe String
    , supportRepositoryName : Maybe String
    , supportRepositoryUrl : Maybe String
    }


decoder : Decoder EditableClientConfig
decoder =
    D.succeed EditableClientConfig
        |> D.required "appTitle" (D.maybe D.string)
        |> D.required "appTitleShort" (D.maybe D.string)
        |> D.required "welcomeInfo" (D.maybe D.string)
        |> D.required "welcomeWarning" (D.maybe D.string)
        |> D.required "loginInfo" (D.maybe D.string)
        |> D.required "dashboard" (D.maybe DashboardWidget.dictDecoder)
        |> D.required "privacyUrl" (D.maybe D.string)
        |> D.required "customMenuLinks" (D.list CustomMenuLink.decoder)
        |> D.required "supportEmail" (D.maybe D.string)
        |> D.required "supportRepositoryName" (D.maybe D.string)
        |> D.required "supportRepositoryUrl" (D.maybe D.string)


encode : EditableClientConfig -> E.Value
encode config =
    E.object
        [ ( "appTitle", E.maybe E.string config.appTitle )
        , ( "appTitleShort", E.maybe E.string config.appTitleShort )
        , ( "welcomeInfo", E.maybe E.string config.welcomeInfo )
        , ( "welcomeWarning", E.maybe E.string config.welcomeWarning )
        , ( "loginInfo", E.maybe E.string config.loginInfo )
        , ( "dashboard", E.maybe (E.dict identity (E.list DashboardWidget.encode)) config.dashboard )
        , ( "privacyUrl", E.maybe E.string config.privacyUrl )
        , ( "customMenuLinks", E.list CustomMenuLink.encode config.customMenuLinks )
        , ( "supportEmail", E.maybe E.string config.supportEmail )
        , ( "supportRepositoryName", E.maybe E.string config.supportRepositoryName )
        , ( "supportRepositoryUrl", E.maybe E.string config.supportRepositoryUrl )
        ]
