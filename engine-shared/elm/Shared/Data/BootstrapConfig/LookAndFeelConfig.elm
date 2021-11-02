module Shared.Data.BootstrapConfig.LookAndFeelConfig exposing
    ( LookAndFeelConfig
    , decoder
    , default
    , defaultAppTitle
    , defaultAppTitleShort
    , encode
    , getAppTitle
    , getAppTitleShort
    , initEmptyForm
    , initForm
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.BootstrapConfig.LookAndFeelConfig.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias LookAndFeelConfig =
    { appTitle : Maybe String
    , appTitleShort : Maybe String
    , stylePrimaryColor : Maybe String
    , styleIllustrationsColor : Maybe String
    , customMenuLinks : List CustomMenuLink
    , loginInfo : Maybe String
    }


default : LookAndFeelConfig
default =
    { appTitle = Nothing
    , appTitleShort = Nothing
    , stylePrimaryColor = Nothing
    , styleIllustrationsColor = Nothing
    , customMenuLinks = []
    , loginInfo = Nothing
    }


defaultAppTitle : String
defaultAppTitle =
    "{defaultAppTitle}"


defaultAppTitleShort : String
defaultAppTitleShort =
    "{defaultAppTitleShort}"


getAppTitle : LookAndFeelConfig -> String
getAppTitle config =
    Maybe.withDefault defaultAppTitle config.appTitle


getAppTitleShort : LookAndFeelConfig -> String
getAppTitleShort config =
    Maybe.withDefault defaultAppTitleShort config.appTitleShort



-- JSON


decoder : Decoder LookAndFeelConfig
decoder =
    D.succeed LookAndFeelConfig
        |> D.required "appTitle" (D.maybe D.string)
        |> D.required "appTitleShort" (D.maybe D.string)
        |> D.optional "stylePrimaryColor" (D.maybe D.string) Nothing
        |> D.optional "styleIllustrationsColor" (D.maybe D.string) Nothing
        |> D.required "customMenuLinks" (D.list CustomMenuLink.decoder)
        |> D.required "loginInfo" (D.maybe D.string)


encode : LookAndFeelConfig -> E.Value
encode config =
    E.object
        [ ( "appTitle", E.maybe E.string config.appTitle )
        , ( "appTitleShort", E.maybe E.string config.appTitleShort )
        , ( "customMenuLinks", E.list CustomMenuLink.encode config.customMenuLinks )
        , ( "loginInfo", E.maybe E.string config.loginInfo )
        ]



-- Form


validation : Validation FormError LookAndFeelConfig
validation =
    V.succeed LookAndFeelConfig
        |> V.andMap (V.field "appTitle" V.maybeString)
        |> V.andMap (V.field "appTitleShort" V.maybeString)
        |> V.andMap (V.field "stylePrimaryColor" V.maybeString)
        |> V.andMap (V.field "styleIllustrationsColor" V.maybeString)
        |> V.andMap (V.field "customMenuLinks" (V.list CustomMenuLink.validation))
        |> V.andMap (V.field "loginInfo" V.maybeString)


initEmptyForm : Form FormError LookAndFeelConfig
initEmptyForm =
    Form.initial [] validation


initForm : LookAndFeelConfig -> Form FormError LookAndFeelConfig
initForm config =
    let
        customMenuLinks =
            List.map
                (\l ->
                    Field.group
                        [ ( "icon", Field.string l.icon )
                        , ( "title", Field.string l.title )
                        , ( "url", Field.string l.url )
                        , ( "newWindow", Field.bool l.newWindow )
                        ]
                )
                config.customMenuLinks

        fields =
            [ ( "appTitle", Field.maybeString config.appTitle )
            , ( "appTitleShort", Field.maybeString config.appTitleShort )
            , ( "stylePrimaryColor", Field.maybeString config.stylePrimaryColor )
            , ( "styleIllustrationsColor", Field.maybeString config.styleIllustrationsColor )
            , ( "customMenuLinks", Field.list customMenuLinks )
            , ( "loginInfo", Field.maybeString config.loginInfo )
            ]
    in
    Form.initial fields validation
