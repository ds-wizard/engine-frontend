module Wizard.Pages.Settings.Common.Forms.OpenIdClientForm exposing
    ( FormMode
    , OpenIdClientForm
    , fillFromDetail
    , init
    , initEmpty
    , isEmpty
    , isMicrosoftMode
    , setFormModeCustomMsg
    , setFormModeMicrosoftMsg
    , toOpenIdClientDetail
    , validation
    )

import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.OpenIdClientDetail as EditableOpenIDServiceConfig exposing (OpenIdClientDetail)
import Wizard.Data.AppState exposing (AppState)


type alias OpenIdClientForm =
    { name : String
    , url : String
    , clientId : String
    , clientSecret : String
    , directoryId : String
    , parameters : List EditableOpenIDServiceConfig.Parameter
    , registrationEnabled : Bool
    , scopeEmail : Bool
    , scopeProfile : Bool
    , styleBackground : Maybe String
    , styleColor : Maybe String
    , styleIcon : Maybe String
    , formMode : FormMode
    }


type FormMode
    = MicrosoftMode
    | CustomMode


formModeToString : FormMode -> String
formModeToString mode =
    case mode of
        MicrosoftMode ->
            "MicrosoftMode"

        CustomMode ->
            "CustomMode"


setFormModeMicrosoftMsg : Form.Msg
setFormModeMicrosoftMsg =
    setFormModeMsg MicrosoftMode


setFormModeCustomMsg : Form.Msg
setFormModeCustomMsg =
    setFormModeMsg CustomMode


setFormModeMsg : FormMode -> Form.Msg
setFormModeMsg mode =
    Form.Input "formMode" Form.Text (Field.String (formModeToString mode))


isMicrosoftMode : Form FormError OpenIdClientForm -> Bool
isMicrosoftMode form =
    (Form.getFieldAsString "formMode" form).value
        |> Maybe.withDefault ""
        |> (\str -> str == formModeToString MicrosoftMode)


initEmpty : AppState -> Form FormError OpenIdClientForm
initEmpty appState =
    Form.initial
        [ ( "formMode", Field.string (formModeToString MicrosoftMode) )
        , ( "registrationEnabled", Field.bool True )
        , ( "scopeEmail", Field.bool True )
        , ( "scopeProfile", Field.bool True )
        ]
        (validation appState)


init : AppState -> OpenIdClientDetail -> Form FormError OpenIdClientForm
init appState detail =
    Form.initial (detailToFormInitials detail) (validation appState)


validation : AppState -> Validation FormError OpenIdClientForm
validation appState =
    let
        validateParameter =
            V.succeed EditableOpenIDServiceConfig.Parameter
                |> V.andMap (V.field "name" V.string)
                |> V.andMap (V.field "value" V.string)

        validateInMode mode fieldValidation defaultValue =
            V.field "formMode" V.string
                |> V.andThen
                    (\str ->
                        if str == formModeToString mode then
                            fieldValidation

                        else
                            V.succeed defaultValue
                    )

        validateClientId =
            V.field "formMode" V.string
                |> V.andThen
                    (\str ->
                        if str == formModeToString MicrosoftMode then
                            V.field "clientId" (V.map Uuid.toString V.uuid)

                        else
                            V.field "clientId" V.string
                    )
    in
    V.succeed OpenIdClientForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (validateInMode CustomMode (V.field "url" (V.url appState)) "")
        |> V.andMap validateClientId
        |> V.andMap (V.field "clientSecret" V.string)
        |> V.andMap (validateInMode MicrosoftMode (V.field "directoryId" (V.map Uuid.toString V.uuid)) "")
        |> V.andMap (V.field "parameters" (V.list validateParameter))
        |> V.andMap (V.field "registrationEnabled" V.bool)
        |> V.andMap (V.field "scopeEmail" V.bool)
        |> V.andMap (V.field "scopeProfile" V.bool)
        |> V.andMap (V.field "styleBackground" V.maybeString)
        |> V.andMap (V.field "styleColor" V.maybeString)
        |> V.andMap (V.field "styleIcon" V.maybeString)
        |> V.andMap (V.field "formMode" validateFormMode)


validateFormMode : Validation FormError FormMode
validateFormMode =
    V.string
        |> V.andThen
            (\str ->
                if str == formModeToString MicrosoftMode then
                    V.succeed MicrosoftMode

                else
                    V.succeed CustomMode
            )


detailToFormInitials : OpenIdClientDetail -> List ( String, Field )
detailToFormInitials detail =
    let
        parameters =
            detail.parameters
                |> List.map
                    (\p ->
                        Field.group
                            [ ( "name", Field.string p.name )
                            , ( "value", Field.string p.value )
                            ]
                    )

        formMode =
            if String.startsWith "https://login.microsoftonline.com/" detail.url then
                MicrosoftMode

            else
                CustomMode

        url =
            case formMode of
                MicrosoftMode ->
                    ""

                CustomMode ->
                    detail.url

        directoryId =
            case formMode of
                MicrosoftMode ->
                    String.replace "https://login.microsoftonline.com/" "" detail.url
                        |> String.split "/"
                        |> List.head
                        |> Maybe.withDefault ""

                CustomMode ->
                    ""
    in
    [ ( "name", Field.string detail.name )
    , ( "url", Field.string url )
    , ( "clientId", Field.string detail.clientId )
    , ( "clientSecret", Field.string detail.clientSecret )
    , ( "directoryId", Field.string directoryId )
    , ( "parameters", Field.list parameters )
    , ( "registrationEnabled", Field.bool detail.registrationEnabled )
    , ( "scopeEmail", Field.bool detail.scopeEmail )
    , ( "scopeProfile", Field.bool detail.scopeProfile )
    , ( "styleBackground", Field.maybeString detail.style.background )
    , ( "styleColor", Field.maybeString detail.style.color )
    , ( "styleIcon", Field.maybeString detail.style.icon )
    , ( "formMode", Field.string (formModeToString formMode) )
    ]


toOpenIdClientDetail : Uuid -> OpenIdClientForm -> OpenIdClientDetail
toOpenIdClientDetail uuid form =
    let
        url =
            case form.formMode of
                MicrosoftMode ->
                    "https://login.microsoftonline.com/" ++ form.directoryId ++ "/v2.0"

                CustomMode ->
                    form.url
    in
    { uuid = uuid
    , name = form.name
    , url = url
    , clientId = form.clientId
    , clientSecret = form.clientSecret
    , parameters = form.parameters
    , registrationEnabled = form.registrationEnabled
    , scopeEmail = form.scopeEmail
    , scopeProfile = form.scopeProfile
    , style =
        { background = form.styleBackground
        , color = form.styleColor
        , icon = form.styleIcon
        }
    }


isEmpty : Form FormError OpenIdClientForm -> Bool
isEmpty form =
    let
        isFieldEmpty field =
            Maybe.isNothing <| (Form.getFieldAsString field form).value

        isParametersEmpty =
            List.isEmpty <| Form.getListIndexes "parameters" form
    in
    List.all identity
        [ isFieldEmpty "name"
        , isFieldEmpty "url"
        , isFieldEmpty "clientId"
        , isFieldEmpty "clientSecret"
        , isFieldEmpty "directoryId"
        , isFieldEmpty "styleBackground"
        , isFieldEmpty "styleColor"
        , isFieldEmpty "styleIcon"
        , isParametersEmpty
        ]


fillFromDetail : AppState -> OpenIdClientDetail -> Form FormError OpenIdClientForm -> Form FormError OpenIdClientForm
fillFromDetail appState openIDServiceConfig form =
    let
        toFormMsg field value =
            Form.Input field Form.Text (Field.String value)

        toFormBoolMsg field value =
            Form.Input field Form.Checkbox (Field.Bool value)

        toParameterMsgs i parameter =
            [ Form.Append "parameters"
            , toFormMsg ("parameters." ++ String.fromInt i ++ ".name") parameter.name
            , toFormMsg ("parameters." ++ String.fromInt i ++ ".value") parameter.value
            ]

        applyFormMsg formMsg =
            Form.update (validation appState) formMsg

        serviceMsgs =
            [ toFormMsg "name" openIDServiceConfig.name
            , toFormMsg "url" openIDServiceConfig.url
            , toFormMsg "clientId" openIDServiceConfig.clientId
            , toFormMsg "clientSecret" openIDServiceConfig.clientSecret
            , toFormBoolMsg "registrationEnabled" openIDServiceConfig.registrationEnabled
            , toFormBoolMsg "scopeEmail" openIDServiceConfig.scopeEmail
            , toFormBoolMsg "scopeProfile" openIDServiceConfig.scopeProfile
            , toFormMsg "styleBackground" (Maybe.withDefault "" openIDServiceConfig.style.background)
            , toFormMsg "styleColor" (Maybe.withDefault "" openIDServiceConfig.style.color)
            , toFormMsg "styleIcon" (Maybe.withDefault "" openIDServiceConfig.style.icon)
            ]

        parametersMsgs =
            List.concat <|
                List.indexedMap toParameterMsgs openIDServiceConfig.parameters

        msgs =
            serviceMsgs ++ parametersMsgs
    in
    List.foldl applyFormMsg form msgs
