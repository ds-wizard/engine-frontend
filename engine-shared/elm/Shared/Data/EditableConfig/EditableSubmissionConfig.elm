module Shared.Data.EditableConfig.EditableSubmissionConfig exposing
    ( EditableSubmissionConfig
    , decoder
    , encode
    , initEmptyForm
    , initForm
    , validation
    )

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Shared.Utils exposing (getOrganizationAndItemId)


type alias EditableSubmissionConfig =
    { enabled : Bool
    , services : List Service
    }


type alias Service =
    { id : String
    , name : String
    , description : String
    , supportedFormats : List SupportedFormat
    , props : List String
    , request : Request
    }


type alias SupportedFormat =
    { templateId : String
    , formatUuid : String
    }


type alias Request =
    { url : String
    , method : String
    , multipart : Multipart
    , headers : Dict String String
    }


type alias Multipart =
    { enabled : Bool
    , fileName : String
    }



-- JSON


decoder : Decoder EditableSubmissionConfig
decoder =
    D.succeed EditableSubmissionConfig
        |> D.required "enabled" D.bool
        |> D.required "services" (D.list decodeService)


decodeService : Decoder Service
decodeService =
    D.succeed Service
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "supportedFormats" (D.list decodeSupportedFormat)
        |> D.required "props" (D.list D.string)
        |> D.required "request" decodeRequest


decodeSupportedFormat : Decoder SupportedFormat
decodeSupportedFormat =
    D.succeed SupportedFormat
        |> D.required "templateId" D.string
        |> D.required "formatUuid" D.string


decodeRequest : Decoder Request
decodeRequest =
    D.succeed Request
        |> D.required "url" D.string
        |> D.required "method" D.string
        |> D.required "multipart" decodeMultipart
        |> D.required "headers" (D.dict D.string)


decodeMultipart : Decoder Multipart
decodeMultipart =
    D.succeed Multipart
        |> D.required "enabled" D.bool
        |> D.required "fileName" D.string


encode : EditableSubmissionConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "services", E.list encodeService config.services )
        ]


encodeService : Service -> E.Value
encodeService definition =
    E.object
        [ ( "id", E.string definition.id )
        , ( "name", E.string definition.name )
        , ( "description", E.string definition.description )
        , ( "supportedFormats", E.list encodeSupportedFormat definition.supportedFormats )
        , ( "props", E.list E.string definition.props )
        , ( "request", encodeRequest definition.request )
        ]


encodeSupportedFormat : SupportedFormat -> E.Value
encodeSupportedFormat supportedFormat =
    E.object
        [ ( "templateId", E.string supportedFormat.templateId )
        , ( "formatUuid", E.string supportedFormat.formatUuid )
        ]


encodeRequest : Request -> E.Value
encodeRequest request =
    E.object
        [ ( "url", E.string request.url )
        , ( "method", E.string request.method )
        , ( "multipart", encodeMultipart request.multipart )
        , ( "headers", E.dict identity E.string request.headers )
        ]


encodeMultipart : Multipart -> E.Value
encodeMultipart multipart =
    E.object
        [ ( "enabled", E.bool multipart.enabled )
        , ( "fileName", E.string multipart.fileName )
        ]



-- Form


validation : Validation FormError EditableSubmissionConfig
validation =
    V.succeed EditableSubmissionConfig
        |> V.andMap (V.field "enabled" V.bool)
        |> V.andMap (V.field "services" (V.list validateService))


validateService : Validation FormError Service
validateService =
    V.succeed Service
        |> V.andMap (V.field "id" V.string)
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.optionalString)
        |> V.andMap (V.field "supportedFormats" (V.list validateSupportedFormat))
        |> V.andMap (V.field "props" (V.list V.string))
        |> V.andMap (V.field "request" validateRequest)


validateSupportedFormat : Validation FormError SupportedFormat
validateSupportedFormat =
    V.succeed SupportedFormat
        |> V.andMap (V.field "templateId" V.string)
        |> V.andMap (V.field "formatUuid" V.string)


validateRequest : Validation FormError Request
validateRequest =
    V.succeed Request
        |> V.andMap (V.field "url" V.string)
        |> V.andMap (V.field "method" V.string)
        |> V.andMap (V.field "multipart" validateMultipart)
        |> V.andMap (V.field "headers" (V.dict V.string))


validateMultipart : Validation FormError Multipart
validateMultipart =
    V.succeed Multipart
        |> V.andMap (V.field "enabled" V.bool)
        |> V.andMap (V.field "enabled" V.bool |> V.ifElse "fileName" V.string V.optionalString)


initEmptyForm : Form FormError EditableSubmissionConfig
initEmptyForm =
    Form.initial [] validation


initForm : EditableSubmissionConfig -> Form FormError EditableSubmissionConfig
initForm config =
    Form.initial (initConfig config) validation


initConfig : EditableSubmissionConfig -> List ( String, Field )
initConfig config =
    let
        services =
            List.map (Field.group << initService) config.services
    in
    [ ( "enabled", Field.bool config.enabled )
    , ( "services", Field.list services )
    ]


initService : Service -> List ( String, Field )
initService definition =
    let
        supportedFormats =
            List.map (Field.group << initSupportedFormat) definition.supportedFormats

        props =
            List.map Field.string definition.props
    in
    [ ( "id", Field.string definition.id )
    , ( "name", Field.string definition.name )
    , ( "description", Field.string definition.description )
    , ( "supportedFormats", Field.list supportedFormats )
    , ( "props", Field.list props )
    , ( "request", Field.group (initRequest definition.request) )
    ]


initSupportedFormat : SupportedFormat -> List ( String, Field )
initSupportedFormat supportedFormat =
    [ ( "template", Field.string (getOrganizationAndItemId supportedFormat.templateId) )
    , ( "templateId", Field.string supportedFormat.templateId )
    , ( "formatUuid", Field.string supportedFormat.formatUuid )
    ]


initRequest : Request -> List ( String, Field )
initRequest request =
    [ ( "url", Field.string request.url )
    , ( "method", Field.string request.method )
    , ( "multipart", Field.group (initMultipart request.multipart) )
    , ( "headers", Field.dict Field.string request.headers )
    ]


initMultipart : Multipart -> List ( String, Field )
initMultipart multipart =
    [ ( "enabled", Field.bool multipart.enabled )
    , ( "fileName", Field.string multipart.fileName )
    ]
