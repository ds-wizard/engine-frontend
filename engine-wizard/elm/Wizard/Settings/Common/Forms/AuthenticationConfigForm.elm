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
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Form.Validate as V
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig exposing (EditableAuthenticationConfig)
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Common.Forms.OpenIDServiceConfigForm as OpenIDServiceConfigForm exposing (OpenIDServiceConfigForm)


type alias AuthenticationConfigForm =
    { defaultRole : String
    , services : List OpenIDServiceConfigForm
    , registrationEnabled : Bool
    , twoFactorAuthEnabled : Bool
    , twoFactorAuthCodeLength : Int
    , twoFactorAuthExpiration : Int
    }


initEmpty : AppState -> Form FormError AuthenticationConfigForm
initEmpty appState =
    Form.initial [] (validation appState)


init : AppState -> EditableAuthenticationConfig -> Form FormError AuthenticationConfigForm
init appState config =
    Form.initial (configToFormInitials config) (validation appState)


validation : AppState -> Validation FormError AuthenticationConfigForm
validation appState =
    V.succeed AuthenticationConfigForm
        |> V.andMap (V.field "defaultRole" V.string)
        |> V.andMap (V.field "services" (V.list (OpenIDServiceConfigForm.validation appState)))
        |> V.andMap (V.field "registrationEnabled" V.bool)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool |> V.ifElse "twoFactorAuthCodeLength" V.int V.optionalInt)
        |> V.andMap (V.field "twoFactorAuthEnabled" V.bool |> V.ifElse "twoFactorAuthExpiration" V.int V.optionalInt)


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
    , ( "twoFactorAuthEnabled", Field.bool config.internal.twoFactorAuth.enabled )
    , ( "twoFactorAuthCodeLength", Field.string (String.fromInt config.internal.twoFactorAuth.codeLength) )
    , ( "twoFactorAuthExpiration", Field.string (String.fromInt config.internal.twoFactorAuth.expiration) )
    ]


toEditableAuthConfig : AuthenticationConfigForm -> EditableAuthenticationConfig
toEditableAuthConfig form =
    { defaultRole = form.defaultRole
    , internal =
        { registration = { enabled = form.registrationEnabled }
        , twoFactorAuth =
            { enabled = form.twoFactorAuthEnabled
            , codeLength = form.twoFactorAuthCodeLength
            , expiration = form.twoFactorAuthExpiration
            }
        }
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


fillOpenIDServiceConfig : AppState -> Int -> EditableOpenIDServiceConfig -> Form FormError AuthenticationConfigForm -> Form FormError AuthenticationConfigForm
fillOpenIDServiceConfig appState index openIDServiceConfig form =
    let
        toFormMsg field value =
            Form.Input ("services." ++ String.fromInt index ++ "." ++ field) Form.Text (Field.String value)

        toParameterMsgs i parameter =
            [ Form.Append ("services." ++ String.fromInt index ++ ".parameters")
            , toFormMsg ("parameters." ++ String.fromInt i ++ ".name") parameter.name
            , toFormMsg ("parameters." ++ String.fromInt i ++ ".value") parameter.value
            ]

        applyFormMsg formMsg =
            Form.update (validation appState) formMsg

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
