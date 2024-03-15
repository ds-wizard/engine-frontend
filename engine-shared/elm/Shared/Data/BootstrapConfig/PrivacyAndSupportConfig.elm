module Shared.Data.BootstrapConfig.PrivacyAndSupportConfig exposing
    ( PrivacyAndSupportConfig
    , decoder
    , default
    , defaultSupportEmail
    , defaultSupportSiteIcon
    , defaultSupportSiteName
    , defaultSupportSiteUrl
    , encode
    , getSupportEmail
    , getSupportSiteIcon
    , getSupportSiteName
    , getSupportSiteUrl
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
    , supportSiteName : Maybe String
    , supportSiteUrl : Maybe String
    , supportSiteIcon : Maybe String
    }


default : PrivacyAndSupportConfig
default =
    { privacyUrl = Nothing
    , termsOfServiceUrl = Nothing
    , supportEmail = Nothing
    , supportSiteName = Nothing
    , supportSiteUrl = Nothing
    , supportSiteIcon = Nothing
    }


getSupportEmail : PrivacyAndSupportConfig -> String
getSupportEmail config =
    Maybe.withDefault defaultSupportEmail config.supportEmail


getSupportSiteName : PrivacyAndSupportConfig -> String
getSupportSiteName config =
    Maybe.withDefault defaultSupportSiteName config.supportSiteName


getSupportSiteUrl : PrivacyAndSupportConfig -> String
getSupportSiteUrl config =
    Maybe.withDefault defaultSupportSiteUrl config.supportSiteUrl


getSupportSiteIcon : PrivacyAndSupportConfig -> String
getSupportSiteIcon config =
    Maybe.withDefault defaultSupportSiteIcon config.supportSiteIcon


defaultSupportEmail : String
defaultSupportEmail =
    "{defaultSupportEmail}"


defaultSupportSiteName : String
defaultSupportSiteName =
    "{defaultSupportRepositoryName}"


defaultSupportSiteUrl : String
defaultSupportSiteUrl =
    "{defaultSupportRepositoryUrl}"


defaultSupportSiteIcon : String
defaultSupportSiteIcon =
    "{defaultSupportSiteIcon}"



-- JSON


decoder : Decoder PrivacyAndSupportConfig
decoder =
    D.succeed PrivacyAndSupportConfig
        |> D.required "privacyUrl" (D.maybe D.string)
        |> D.required "termsOfServiceUrl" (D.maybe D.string)
        |> D.required "supportEmail" (D.maybe D.string)
        |> D.required "supportSiteName" (D.maybe D.string)
        |> D.required "supportSiteUrl" (D.maybe D.string)
        |> D.required "supportSiteIcon" (D.maybe D.string)


encode : PrivacyAndSupportConfig -> E.Value
encode config =
    E.object
        [ ( "privacyUrl", E.maybe E.string config.privacyUrl )
        , ( "termsOfServiceUrl", E.maybe E.string config.termsOfServiceUrl )
        , ( "supportEmail", E.maybe E.string config.supportEmail )
        , ( "supportSiteName", E.maybe E.string config.supportSiteName )
        , ( "supportSiteUrl", E.maybe E.string config.supportSiteUrl )
        , ( "supportSiteIcon", E.maybe E.string config.supportSiteIcon )
        ]



-- Form


validation : Validation FormError PrivacyAndSupportConfig
validation =
    V.succeed PrivacyAndSupportConfig
        |> V.andMap (V.field "privacyUrl" V.maybeString)
        |> V.andMap (V.field "termsOfServiceUrl" V.maybeString)
        |> V.andMap (V.field "supportEmail" V.maybeString)
        |> V.andMap (V.field "supportSiteName" V.maybeString)
        |> V.andMap (V.field "supportSiteUrl" V.maybeString)
        |> V.andMap (V.field "supportSiteIcon" V.maybeString)


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
            , ( "supportSiteName", Field.maybeString config.supportSiteName )
            , ( "supportSiteUrl", Field.maybeString config.supportSiteUrl )
            , ( "supportSiteIcon", Field.maybeString config.supportSiteIcon )
            ]
    in
    Form.initial fields validation
