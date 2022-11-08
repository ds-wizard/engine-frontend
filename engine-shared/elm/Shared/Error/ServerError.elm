module Shared.Error.ServerError exposing
    ( Message
    , ServerError(..)
    , SystemLogErrorData
    , UserFormErrorData
    , decoder
    , forbiddenMessage
    , messageToReadable
    )

import Dict exposing (Dict)
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import String.Format as String


type ServerError
    = UserSimpleError Message
    | UserFormError UserFormErrorData
    | ForbiddenError
    | SystemLogError SystemLogErrorData


type alias Message =
    { code : String
    , params : List String
    }


type alias UserFormErrorData =
    { formErrors : List Message
    , fieldErrors : Dict String (List Message)
    }


type alias SystemLogErrorData =
    { defaultMessage : String
    , params : List String
    }


decoder : Decoder ServerError
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ServerError
decoderByType errorType =
    case errorType of
        "UserSimpleError" ->
            userSimpleErrorDecoder

        "UserFormError" ->
            userFormErrorDecoder

        "SystemLogError" ->
            systemErrorDecoder

        _ ->
            D.fail <| "Unknown error type " ++ errorType


userSimpleErrorDecoder : Decoder ServerError
userSimpleErrorDecoder =
    D.succeed UserSimpleError
        |> D.required "error" messageDecoder


userFormErrorDecoder : Decoder ServerError
userFormErrorDecoder =
    D.succeed UserFormErrorData
        |> D.required "formErrors" (D.list messageDecoder)
        |> D.required "fieldErrors" (D.dict (D.list messageDecoder))
        |> D.map UserFormError


systemErrorDecoder : Decoder ServerError
systemErrorDecoder =
    D.succeed SystemLogError
        |> D.required "error" systemLogErrorDataDecoder


messageDecoder : Decoder Message
messageDecoder =
    D.succeed Message
        |> D.required "code" D.string
        |> D.required "params" (D.list D.string)


systemLogErrorDataDecoder : Decoder SystemLogErrorData
systemLogErrorDataDecoder =
    D.succeed SystemLogErrorData
        |> D.required "defaultMessage" D.string
        |> D.required "params" (D.list D.string)


forbiddenMessage : { a | locale : Gettext.Locale } -> String
forbiddenMessage appState =
    gettext "You do not have permission to view this page." appState.locale


messageToReadable : { a | locale : Gettext.Locale } -> Message -> Maybe String
messageToReadable appState message =
    case message.code of
        -- Shared
        "error.validation.km_id_uniqueness" ->
            Just <| gettext "Knowledge Model ID is already used." appState.locale

        "error.validation.pkg_id_uniqueness" ->
            Just <| gettext "Knowledge Model already exists." appState.locale

        "error.validation.tml_id_uniqueness" ->
            Just <| gettext "Document template already exists." appState.locale

        "error.validation.lcl_id_uniqueness" ->
            Just <| gettext "Locale already exists." appState.locale

        "error.validation.user_email_uniqueness" ->
            Just <| gettext "Email is already used." appState.locale

        "error.service.pkg.highest_number_in_new_version" ->
            Just <| gettext "New version has to be higher than the previous one." appState.locale

        "error.service.tb.missing_template_json" ->
            Just <| gettext "\"template.json\" was not found in archive." appState.locale

        "error.service.tb.unable_to_decode_template_json" ->
            Just <| String.format (gettext "Error while parsing template.json: \"%s\"." appState.locale) message.params

        "error.service.tb.missing_asset" ->
            Just <| String.format (gettext "Asset \"%s\" was not found in archive." appState.locale) message.params

        -- Registry
        "error.validation.email_uniqueness" ->
            Just <| gettext "Email is already used." appState.locale

        "error.validation.hash_absence" ->
            Just <| gettext "Link is not valid." appState.locale

        "error.validation.organization_email_absence" ->
            Just <| gettext "This email is not connected to any organization." appState.locale

        "error.validation.organization_email_uniqueness" ->
            Just <| gettext "Organization email is already used." appState.locale

        "error.validation.organization_id_uniqueness" ->
            Just <| gettext "Organization ID is already used." appState.locale

        "error.service.organization.required_hash_in_query_params" ->
            Just <| gettext "A hash query param has to be provided." appState.locale

        -- Wizard
        "error.validation.app_id_uniqueness" ->
            Just <| gettext "App ID is already used." appState.locale

        "error.validation.openid_code_absence" ->
            Just <| gettext "Authentication Code is not provided." appState.locale

        "error.validation.openid_profile_info_absence" ->
            Just <| gettext "Profile Information from OpenID service is missing." appState.locale

        "error.validation.tml_deletation" ->
            Just <| gettext "Document template cannot be deleted because it is used in some projects or documents." appState.locale

        "error.validation.tml_unsupported_version" ->
            Just <| gettext "Document template metamodel version is not supported." appState.locale

        "error.service.app.limit_exceeded" ->
            Just <| String.format (gettext "Limit of %s reached (current: %s, limit: %s)" appState.locale) message.params

        "error.service.lb.missing_locale_json" ->
            Just <| gettext "\"locale.json\" was not found in archive." appState.locale

        "error.service.lb.unable_to_decode_locale_json" ->
            Just <| String.format (gettext "Error while parsing locale.json: \"%s\"." appState.locale) message.params

        "error.service.lb.missing_file" ->
            Just <| String.format (gettext "File \"%s\" was not found in archive." appState.locale) message.params

        "error.service.lb.pull_non_existing_locale" ->
            Just <| gettext "The locale not found in Registry" appState.locale

        "error.service.pkg.pkg_cant_be_deleted_because_it_is_used_by_some_other_entity" ->
            Just <| gettext "Knowledge Model cannot be deleted because it is used in some Projects or Knowledge Model Editors." appState.locale

        "error.service.pb.pull_non_existing_pkg" ->
            Just <| gettext "The Knowledge Model was not found in the Registry." appState.locale

        "error.service.qtn.qtn_cant_be_deleted_because_it_is_used_in_migration" ->
            Just <| gettext "Project cannot be deleted because it is used in some project migration." appState.locale

        "error.service.tb.pull_non_existing_tml" ->
            Just <| gettext "The document template was not found in the Registry." appState.locale

        "error.service.token.Incorrect_email_or_password" ->
            Just <| gettext "Incorrect email or password" appState.locale

        "error.service.token.account_is_not_activated" ->
            Just <| gettext "The account is not activated." appState.locale

        "error.service.user.required_admin_role_or_hash_in_query_params" ->
            Just <| gettext "A hash query param has to be provided." appState.locale

        "error.service.user.required_hash_in_query_params" ->
            Just <| gettext "A hash query param has to be provided." appState.locale

        _ ->
            Nothing
