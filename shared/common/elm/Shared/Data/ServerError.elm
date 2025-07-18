module Shared.Data.ServerError exposing
    ( Message
    , ServerError(..)
    , SystemLogErrorData
    , UserFormErrorData
    , decoder
    , forbiddenMessage
    , getUserFormErrorData
    , messageToReadable
    )

import Dict exposing (Dict)
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Shared.Utils.ByteUnits as ByteUnits
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


getUserFormErrorData : ServerError -> Maybe UserFormErrorData
getUserFormErrorData serverError =
    case serverError of
        UserFormError data ->
            Just data

        _ ->
            Nothing


decoder : Decoder ServerError
decoder =
    D.oneOf
        [ D.field "type" D.string |> D.andThen decoderByType
        , defaultErrorDecoder
        ]


defaultErrorDecoder : Decoder ServerError
defaultErrorDecoder =
    D.succeed UserSimpleError
        |> D.required "message" messageDecoder


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
            Just <| gettext "Knowledge model ID is already used." appState.locale

        "error.validation.pkg_id_uniqueness" ->
            Just <| gettext "Knowledge model already exists." appState.locale

        "error.validation.pkg_unsupported_metamodel_version" ->
            Just <| gettext "Knowledge model metamodel version is not supported." appState.locale

        "error.validation.tml_id_uniqueness" ->
            Just <| gettext "Document template already exists." appState.locale

        "error.validation.lcl_id_uniqueness" ->
            Just <| gettext "Locale already exists." appState.locale

        "error.validation.user_email_uniqueness" ->
            Just <| gettext "Email is already used." appState.locale

        "error.validation.qtn_migration_uniqueness" ->
            Just <| gettext "There is already an ongoing project migration." appState.locale

        "error.service.pkg.highest_number_in_new_version" ->
            Just <| gettext "New version has to be higher than the previous one." appState.locale

        "error.service.tb.missing_template_json" ->
            Just <| gettext "\"template.json\" was not found in archive." appState.locale

        "error.service.tb.unable_to_decode_template_json" ->
            Just <| String.format (gettext "Error while parsing template.json: \"%s\"." appState.locale) message.params

        "error.service.tb.missing_asset" ->
            Just <| String.format (gettext "Asset \"%s\" was not found in archive." appState.locale) message.params

        -- Registry
        "error.api.common.unable_to_get_organization" ->
            Just <| gettext "Invalid credentials." appState.locale

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
        "error.database.entity_not_found" ->
            case List.head message.params of
                Just "branch" ->
                    Just <| gettext "Knowledge model editor not found." appState.locale

                Just "document" ->
                    Just <| gettext "Document not found." appState.locale

                Just "document_template" ->
                    Just <| gettext "Document template not found." appState.locale

                Just "document_template_draft" ->
                    Just <| gettext "Document template draft not found." appState.locale

                Just "document_template_asset" ->
                    Just <| gettext "Document template asset not found." appState.locale

                Just "document_template_file" ->
                    Just <| gettext "Document template file not found." appState.locale

                Just "knowledge_model_migration" ->
                    Just <| gettext "Knowledge model migration not found." appState.locale

                Just "locale" ->
                    Just <| gettext "Locale not found." appState.locale

                Just "package" ->
                    Just <| gettext "Knowledge model not found." appState.locale

                Just "questionnaire" ->
                    Just <| gettext "Project not found." appState.locale

                Just "questionnaire_action" ->
                    Just <| gettext "Project action not found." appState.locale

                Just "questionnaire_comment" ->
                    Just <| gettext "Comment not found." appState.locale

                Just "questionnaire_comment_thread" ->
                    Just <| gettext "Comment thread not found." appState.locale

                Just "questionnaire_file" ->
                    Just <| gettext "Project file not found." appState.locale

                Just "questionnaire_importer" ->
                    Just <| gettext "Project importer not found." appState.locale

                Just "questionnaire_migration" ->
                    Just <| gettext "Project migration not found." appState.locale

                Just "questionnaire_version" ->
                    Just <| gettext "Project version not found." appState.locale

                Just "user_entity" ->
                    Just <| gettext "User not found." appState.locale

                Just "user_group" ->
                    Just <| gettext "User group not found." appState.locale

                _ ->
                    Just <| gettext "Not found." appState.locale

        "error.validation.app_id_uniqueness" ->
            Just <| gettext "App ID is already used." appState.locale

        "error.validation.doc_tml_file_or_asset_uniqueness" ->
            Just <| gettext "File with this name already exists." appState.locale

        "error.validation.openid_code_absence" ->
            Just <| gettext "Authentication code is not provided." appState.locale

        "error.validation.openid_profile_info_absence" ->
            Just <| gettext "Profile Information from OpenID service is missing." appState.locale

        "error.validation.tml_deletation" ->
            Just <| gettext "Document template cannot be deleted because it is used in some projects or documents." appState.locale

        "error.validation.tml_unsupported_metamodel_version" ->
            Just <| gettext "Document template metamodel version is not supported." appState.locale

        "error.service.tenant.limit_exceeded" ->
            case message.params of
                "storage" :: current :: limit :: [] ->
                    let
                        parseBytes =
                            Maybe.unwrap "0" ByteUnits.toReadable << String.toInt
                    in
                    Just <|
                        String.format
                            (gettext "Limit of %s reached (current: %s, limit: %s)." appState.locale)
                            [ gettext "storage" appState.locale, parseBytes current, parseBytes limit ]

                what :: current :: limit :: [] ->
                    let
                        whatTranslated =
                            case what of
                                "users" ->
                                    gettext "users" appState.locale

                                "active users" ->
                                    gettext "active users" appState.locale

                                "branches" ->
                                    gettext "knowledge model editors" appState.locale

                                "knowledge models" ->
                                    gettext "knowledge models" appState.locale

                                "questionnaires" ->
                                    gettext "projects" appState.locale

                                "document templates" ->
                                    gettext "document templates" appState.locale

                                "document template drafts" ->
                                    gettext "document template editors" appState.locale

                                "documents" ->
                                    gettext "documents" appState.locale

                                "locales" ->
                                    gettext "locales" appState.locale

                                _ ->
                                    what
                    in
                    Just <| String.format (gettext "Limit of %s reached (current: %s, limit: %s)." appState.locale) [ whatTranslated, current, limit ]

                _ ->
                    Just <| String.format (gettext "Limit of %s reached (current: %s, limit: %s)." appState.locale) message.params

        "error.service.lb.missing_locale_json" ->
            Just <| gettext "\"locale.json\" was not found in archive." appState.locale

        "error.service.lb.unable_to_decode_locale_json" ->
            Just <| String.format (gettext "Error while parsing locale.json: \"%s\"." appState.locale) message.params

        "error.service.lb.missing_file" ->
            Just <| String.format (gettext "File \"%s\" was not found in archive." appState.locale) message.params

        "error.service.lb.pull_non_existing_locale" ->
            Just <| gettext "The locale not found in Registry." appState.locale

        "error.service.pkg.pkg_cant_be_deleted_because_it_is_used_by_some_other_entity" ->
            Just <| gettext "Knowledge model cannot be deleted because it is used in some projects or knowledge model editors." appState.locale

        "error.service.pb.pull_non_existing_pkg" ->
            Just <| gettext "The knowledge model was not found in the Registry." appState.locale

        "error.service.qtn.qtn_cant_be_deleted_because_it_is_used_in_migration" ->
            Just <| gettext "Project cannot be deleted because it is used in some project migration." appState.locale

        "error.service.tb.pull_non_existing_tml" ->
            Just <| gettext "The document template was not found in the Registry." appState.locale

        "error.service.token.incorrect_email_or_password" ->
            Just <| gettext "Incorrect email or password." appState.locale

        "error.service.token.incorrect_code" ->
            Just <| gettext "Incorrect authentication code." appState.locale

        "error.service.token.account_is_not_activated" ->
            Just <| gettext "The account is not activated." appState.locale

        "error.service.user.required_admin_role_or_hash_in_query_params" ->
            Just <| gettext "A hash query param has to be provided." appState.locale

        "error.service.user.required_hash_in_query_params" ->
            Just <| gettext "A hash query param has to be provided." appState.locale

        _ ->
            Nothing
