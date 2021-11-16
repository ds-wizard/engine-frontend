module Shared.Data.EditableConfig.EditableLookAndFeelConfig exposing
    ( EditableLookAndFeelConfig
    , decoder
    , encode
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


type alias EditableLookAndFeelConfig =
    { appTitle : Maybe String
    , appTitleShort : Maybe String
    , primaryColor : Maybe String
    , illustrationsColor : Maybe String
    , customMenuLinks : List CustomMenuLink
    , loginInfo : Maybe String
    , logoUrl : Maybe String
    , styleUrl : Maybe String
    }



-- JSON


decoder : Decoder EditableLookAndFeelConfig
decoder =
    D.succeed EditableLookAndFeelConfig
        |> D.required "appTitle" (D.maybe D.string)
        |> D.required "appTitleShort" (D.maybe D.string)
        |> D.required "primaryColor" (D.maybe D.string)
        |> D.required "illustrationsColor" (D.maybe D.string)
        |> D.required "customMenuLinks" (D.list CustomMenuLink.decoder)
        |> D.required "loginInfo" (D.maybe D.string)
        |> D.required "logoUrl" (D.maybe D.string)
        |> D.required "styleUrl" (D.maybe D.string)


encode : EditableLookAndFeelConfig -> E.Value
encode config =
    E.object
        [ ( "appTitle", E.maybe E.string config.appTitle )
        , ( "appTitleShort", E.maybe E.string config.appTitleShort )
        , ( "primaryColor", E.maybe E.string config.primaryColor )
        , ( "illustrationsColor", E.maybe E.string config.illustrationsColor )
        , ( "customMenuLinks", E.list CustomMenuLink.encode config.customMenuLinks )
        , ( "loginInfo", E.maybe E.string config.loginInfo )
        , ( "logoUrl", E.maybe E.string config.logoUrl )
        , ( "styleUrl", E.maybe E.string config.styleUrl )
        ]



-- Form


validation : Validation FormError EditableLookAndFeelConfig
validation =
    V.succeed EditableLookAndFeelConfig
        |> V.andMap (V.field "appTitle" V.maybeString)
        |> V.andMap (V.field "appTitleShort" V.maybeString)
        |> V.andMap (V.field "stylePrimaryColor" V.maybeString)
        |> V.andMap (V.field "styleIllustrationsColor" V.maybeString)
        |> V.andMap (V.field "customMenuLinks" (V.list CustomMenuLink.validation))
        |> V.andMap (V.field "loginInfo" V.maybeString)
        |> V.andMap (V.field "logoUrl" V.maybeString)
        |> V.andMap (V.field "styleUrl" V.maybeString)


initEmptyForm : Form FormError EditableLookAndFeelConfig
initEmptyForm =
    Form.initial [] validation


initForm : EditableLookAndFeelConfig -> Form FormError EditableLookAndFeelConfig
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
            , ( "stylePrimaryColor", Field.maybeString config.primaryColor )
            , ( "styleIllustrationsColor", Field.maybeString config.illustrationsColor )
            , ( "customMenuLinks", Field.list customMenuLinks )
            , ( "loginInfo", Field.maybeString config.loginInfo )
            , ( "logoUrl", Field.maybeString config.logoUrl )
            , ( "styleUrl", Field.maybeString config.styleUrl )
            ]
    in
    Form.initial fields validation
