module Shared.Data.BootstrapConfig.PrivacyAndSupportConfig exposing
    ( PrivacyAndSupportConfig
    , decoder
    , default
    , defaultPrivacyUrl
    , defaultSupportEmail
    , defaultSupportRepositoryName
    , defaultSupportRepositoryUrl
    , defaultTermsOfServiceUrl
    , encode
    , getPrivacyUrl
    , getSupportEmail
    , getSupportRepositoryName
    , getSupportRepositoryUrl
    , getTermsOfServiceUrl
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
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias PrivacyAndSupportConfig =
    { privacyUrl : Maybe String
    , termsOfServiceUrl : Maybe String
    , supportEmail : Maybe String
    , supportRepositoryName : Maybe String
    , supportRepositoryUrl : Maybe String
    }


default : PrivacyAndSupportConfig
default =
    { privacyUrl = Nothing
    , termsOfServiceUrl = Nothing
    , supportEmail = Nothing
    , supportRepositoryName = Nothing
    , supportRepositoryUrl = Nothing
    }


getPrivacyUrl : PrivacyAndSupportConfig -> String
getPrivacyUrl config =
    Maybe.withDefault defaultPrivacyUrl config.privacyUrl


getTermsOfServiceUrl : PrivacyAndSupportConfig -> String
getTermsOfServiceUrl config =
    Maybe.withDefault defaultTermsOfServiceUrl config.termsOfServiceUrl


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


defaultTermsOfServiceUrl : String
defaultTermsOfServiceUrl =
    "{defaultTermsOfServiceUrl}"


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
        |> D.required "termsOfServiceUrl" (D.maybe D.string)
        |> D.required "supportEmail" (D.maybe D.string)
        |> D.required "supportRepositoryName" (D.maybe D.string)
        |> D.required "supportRepositoryUrl" (D.maybe D.string)


encode : PrivacyAndSupportConfig -> E.Value
encode config =
    E.object
        [ ( "privacyUrl", E.maybe E.string config.privacyUrl )
        , ( "termsOfServiceUrl", E.maybe E.string config.termsOfServiceUrl )
        , ( "supportEmail", E.maybe E.string config.supportEmail )
        , ( "supportRepositoryName", E.maybe E.string config.supportRepositoryName )
        , ( "supportRepositoryUrl", E.maybe E.string config.supportRepositoryUrl )
        ]



-- Form


validation : Validation FormError PrivacyAndSupportConfig
validation =
    V.succeed PrivacyAndSupportConfig
        |> V.andMap (V.field "privacyUrl" V.maybeString)
        |> V.andMap (V.field "termsOfServiceUrl" V.maybeString)
        |> V.andMap (V.field "supportEmail" V.maybeString)
        |> V.andMap (V.field "supportRepositoryName" V.maybeString)
        |> V.andMap (V.field "supportRepositoryUrl" V.maybeString)


initEmptyForm : Form FormError PrivacyAndSupportConfig
initEmptyForm =
    Form.initial [] validation


initForm : PrivacyAndSupportConfig -> Form FormError PrivacyAndSupportConfig
initForm config =
    let
        fields =
            [ ( "privacyUrl", Field.maybeString config.privacyUrl )
            , ( "termsOfServiceUrl", Field.maybeString config.termsOfServiceUrl )
            , ( "supportEmail", Field.maybeString config.supportEmail )
            , ( "supportRepositoryName", Field.maybeString config.supportRepositoryName )
            , ( "supportRepositoryUrl", Field.maybeString config.supportRepositoryUrl )
            ]
    in
    Form.initial fields validation
