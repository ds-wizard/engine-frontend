module Wizard.Common.Config.PrivacyAndSupportConfig exposing
    ( PrivacyAndSupportConfig
    , decoder
    , default
    , defaultPrivacyUrl
    , defaultSupportEmail
    , defaultSupportRepositoryName
    , defaultSupportRepositoryUrl
    , encode
    , getPrivacyUrl
    , getSupportEmail
    , getSupportRepositoryName
    , getSupportRepositoryUrl
    , initEmptyForm
    , initForm
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Field as Field
import Wizard.Common.Form.Validate as V


type alias PrivacyAndSupportConfig =
    { privacyUrl : Maybe String
    , supportEmail : Maybe String
    , supportRepositoryName : Maybe String
    , supportRepositoryUrl : Maybe String
    }


default : PrivacyAndSupportConfig
default =
    { privacyUrl = Nothing
    , supportEmail = Nothing
    , supportRepositoryName = Nothing
    , supportRepositoryUrl = Nothing
    }


getPrivacyUrl : PrivacyAndSupportConfig -> String
getPrivacyUrl config =
    Maybe.withDefault defaultPrivacyUrl config.privacyUrl


getSupportEmail : PrivacyAndSupportConfig -> String
getSupportEmail config =
    Maybe.withDefault defaultSupportEmail config.supportEmail


getSupportRepositoryName : PrivacyAndSupportConfig -> String
getSupportRepositoryName config =
    Maybe.withDefault defaultSupportRepositoryName config.supportRepositoryName


getSupportRepositoryUrl : PrivacyAndSupportConfig -> String
getSupportRepositoryUrl config =
    Maybe.withDefault defaultSupportRepositoryUrl config.supportRepositoryUrl


defaultPrivacyUrl : String
defaultPrivacyUrl =
    "{defaultPrivacyUrl}"


defaultSupportEmail : String
defaultSupportEmail =
    "{defaultSupportEmail}"


defaultSupportRepositoryName : String
defaultSupportRepositoryName =
    "{defaultSupportRepositoryName}"


defaultSupportRepositoryUrl : String
defaultSupportRepositoryUrl =
    "{defaultSupportRepositoryUrl}"



-- JSON


decoder : Decoder PrivacyAndSupportConfig
decoder =
    D.succeed PrivacyAndSupportConfig
        |> D.required "privacyUrl" (D.maybe D.string)
        |> D.required "supportEmail" (D.maybe D.string)
        |> D.required "supportRepositoryName" (D.maybe D.string)
        |> D.required "supportRepositoryUrl" (D.maybe D.string)


encode : PrivacyAndSupportConfig -> E.Value
encode config =
    E.object
        [ ( "privacyUrl", E.maybe E.string config.privacyUrl )
        , ( "supportEmail", E.maybe E.string config.supportEmail )
        , ( "supportRepositoryName", E.maybe E.string config.supportRepositoryName )
        , ( "supportRepositoryUrl", E.maybe E.string config.supportRepositoryUrl )
        ]



-- Form


validation : Validation CustomFormError PrivacyAndSupportConfig
validation =
    V.succeed PrivacyAndSupportConfig
        |> V.andMap (V.field "privacyUrl" V.maybeString)
        |> V.andMap (V.field "supportEmail" V.maybeString)
        |> V.andMap (V.field "supportRepositoryName" V.maybeString)
        |> V.andMap (V.field "supportRepositoryUrl" V.maybeString)


initEmptyForm : Form CustomFormError PrivacyAndSupportConfig
initEmptyForm =
    Form.initial [] validation


initForm : PrivacyAndSupportConfig -> Form CustomFormError PrivacyAndSupportConfig
initForm config =
    let
        fields =
            [ ( "privacyUrl", Field.maybeString config.privacyUrl )
            , ( "supportEmail", Field.maybeString config.supportEmail )
            , ( "supportRepositoryName", Field.maybeString config.supportRepositoryName )
            , ( "supportRepositoryUrl", Field.maybeString config.supportRepositoryUrl )
            ]
    in
    Form.initial fields validation
