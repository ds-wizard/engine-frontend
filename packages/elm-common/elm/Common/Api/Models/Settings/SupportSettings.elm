module Common.Api.Models.Settings.SupportSettings exposing
    ( SupportSettings
    , decoder
    , default
    , encode
    , getSupportEmail
    , getSupportSiteIcon
    , getSupportSiteName
    , getSupportSiteUrl
    , initEmptyForm
    , initForm
    , section
    , validation
    )

import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias SupportSettings =
    { supportEmail : Maybe String
    , supportSiteName : Maybe String
    , supportSiteUrl : Maybe String
    , supportSiteIcon : Maybe String
    }


default : SupportSettings
default =
    { supportEmail = Nothing
    , supportSiteName = Nothing
    , supportSiteUrl = Nothing
    , supportSiteIcon = Nothing
    }


getSupportEmail : { a | supportEmail : String } -> SupportSettings -> String
getSupportEmail defaults settings =
    Maybe.withDefault defaults.supportEmail settings.supportEmail


getSupportSiteName : { a | supportSiteName : String } -> SupportSettings -> String
getSupportSiteName defaults settings =
    Maybe.withDefault defaults.supportSiteName settings.supportSiteName


getSupportSiteUrl : { a | supportSiteUrl : String } -> SupportSettings -> String
getSupportSiteUrl defaults settings =
    Maybe.withDefault defaults.supportSiteUrl settings.supportSiteUrl


getSupportSiteIcon : { a | supportSiteIcon : String } -> SupportSettings -> String
getSupportSiteIcon defaults settings =
    Maybe.withDefault defaults.supportSiteIcon settings.supportSiteIcon


section : { name : String, decoder : Decoder SupportSettings, encode : SupportSettings -> E.Value }
section =
    { name = "support", decoder = decoder, encode = encode }



-- JSON


decoder : Decoder SupportSettings
decoder =
    D.succeed SupportSettings
        |> D.required "supportEmail" (D.maybe D.string)
        |> D.required "supportSiteName" (D.maybe D.string)
        |> D.required "supportSiteUrl" (D.maybe D.string)
        |> D.required "supportSiteIcon" (D.maybe D.string)


encode : SupportSettings -> E.Value
encode settings =
    E.object
        [ ( "supportEmail", E.maybe E.string settings.supportEmail )
        , ( "supportSiteName", E.maybe E.string settings.supportSiteName )
        , ( "supportSiteUrl", E.maybe E.string settings.supportSiteUrl )
        , ( "supportSiteIcon", E.maybe E.string settings.supportSiteIcon )
        ]



-- Form


validation : Validation FormError SupportSettings
validation =
    V.succeed SupportSettings
        |> V.andMap (V.field "supportEmail" V.maybeString)
        |> V.andMap (V.field "supportSiteName" V.maybeString)
        |> V.andMap (V.field "supportSiteUrl" V.maybeString)
        |> V.andMap (V.field "supportSiteIcon" V.maybeString)


initEmptyForm : Form FormError SupportSettings
initEmptyForm =
    Form.initial [] validation


initForm : SupportSettings -> Form FormError SupportSettings
initForm settings =
    Form.initial
        [ ( "supportEmail", Field.maybeString settings.supportEmail )
        , ( "supportSiteName", Field.maybeString settings.supportSiteName )
        , ( "supportSiteUrl", Field.maybeString settings.supportSiteUrl )
        , ( "supportSiteIcon", Field.maybeString settings.supportSiteIcon )
        ]
        validation
