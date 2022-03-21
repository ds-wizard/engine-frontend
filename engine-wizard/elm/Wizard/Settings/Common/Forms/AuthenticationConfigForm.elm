module Wizard.Settings.Common.Forms.AuthenticationConfigForm exposing
    ( AuthenticationConfigForm
    , fillOpenIDServiceConfig
    , init
    , initEmpty
    , isOpenIDServiceEmpty
    , toEditableAuthConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Maybe.Extra as Maybe
import Shared.Data.EditableConfig.EditableAuthenticationConfig exposing (EditableAuthenticationConfig)
import Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Shared.Form.FormError exposing (FormError)
import Wizard.Settings.Common.Forms.OpenIDServiceConfigForm as OpenIDServiceConfigForm exposing (OpenIDServiceConfigForm)


type alias AuthenticationConfigForm =
    { defaultRole : String
    , services : List OpenIDServiceConfigForm
    , registrationEnabled : Bool
    }


initEmpty : Form FormError AuthenticationConfigForm
initEmpty =
    Form.initial [] validation


init : EditableAuthenticationConfig -> Form FormError AuthenticationConfigForm
init config =
    Form.initial (configToFormInitials config) validation


validation : Validation FormError AuthenticationConfigForm
validation =
    V.succeed AuthenticationConfigForm
        |> V.andMap (V.field "defaultRole" V.string)
        |> V.andMap (V.field "services" (V.list OpenIDServiceConfigForm.validation))
        |> V.andMap (V.field "registrationEnabled" V.bool)


configToFormInitials : EditableAuthenticationConfig -> List ( String, Field )
configToFormInitials config =
    let
        services =
            config.external.services
                |> List.map (Field.group << OpenIDServiceConfigForm.configToFormInitials)
    in
    [ ( "defaultRole", Field.string config.defaultRole )
    , ( "services", Field.list services )
    , ( "registrationEnabled", Field.bool config.internal.registration.enabled )
    ]


toEditableAuthConfig : AuthenticationConfigForm -> EditableAuthenticationConfig
toEditableAuthConfig form =
    { defaultRole = form.defaultRole
    , internal = { registration = { enabled = form.registrationEnabled } }
    , external = { services = List.map OpenIDServiceConfigForm.toEditableOpenIDServiceConfig form.services }
    }


isOpenIDServiceEmpty : Int -> Form FormError AuthenticationConfigForm -> Bool
isOpenIDServiceEmpty index form =
    let
        isFieldEmpty field =
            Maybe.isNothing <| (Form.getFieldAsString ("services." ++ String.fromInt index ++ "." ++ field) form).value

        isParametersEmpty =
            List.isEmpty <| Form.getListIndexes ("services." ++ String.fromInt index ++ ".parameters") form
    in
    List.all identity
        [ isFieldEmpty "id"
        , isFieldEmpty "name"
        , isFieldEmpty "url"
        , isFieldEmpty "clientId"
        , isFieldEmpty "clientSecret"
        , isFieldEmpty "styleBackground"
        , isFieldEmpty "styleColor"
        , isFieldEmpty "styleIcon"
        , isParametersEmpty
        ]


fillOpenIDServiceConfig : Int -> EditableOpenIDServiceConfig -> Form FormError AuthenticationConfigForm -> Form FormError AuthenticationConfigForm
fillOpenIDServiceConfig index openIDServiceConfig form =
    let
        toFormMsg field value =
            Form.Input ("services." ++ String.fromInt index ++ "." ++ field) Form.Text (Field.String value)

        toParameterMsgs i parameter =
            [ Form.Append ("services." ++ String.fromInt index ++ ".parameters")
            , toFormMsg ("parameters." ++ String.fromInt i ++ ".name") parameter.name
            , toFormMsg ("parameters." ++ String.fromInt i ++ ".value") parameter.value
            ]

        applyFormMsg formMsg =
            Form.update validation formMsg

        serviceMsgs =
            [ toFormMsg "id" openIDServiceConfig.id
            , toFormMsg "name" openIDServiceConfig.name
            , toFormMsg "url" openIDServiceConfig.url
            , toFormMsg "clientId" openIDServiceConfig.clientId
            , toFormMsg "clientSecret" openIDServiceConfig.clientSecret
            , toFormMsg "styleBackground" (Maybe.withDefault "" openIDServiceConfig.style.background)
            , toFormMsg "styleColor" (Maybe.withDefault "" openIDServiceConfig.style.color)
            , toFormMsg "styleIcon" (Maybe.withDefault "" openIDServiceConfig.style.icon)
            ]

        parametersMsgs =
            List.foldr (++) [] <|
                List.indexedMap toParameterMsgs openIDServiceConfig.parameters

        msgs =
            serviceMsgs ++ parametersMsgs
    in
    List.foldl applyFormMsg form msgs
