module Common.Api.ServerError exposing
    ( Message
    , ServerError(..)
    , SystemLogErrorData
    , TooManyRequestsErrorData
    , UserFormErrorData
    , decoder
    , forbiddenMessage
    , getUserFormErrorData
    , messageToReadable
    , messageToText
    , tooManyRequestsErrorDecoder
    , tooManyRequestsMessage
    )

import Common.Utils.ByteUnits as ByteUnits
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import String.Format as String


type ServerError
    = UserSimpleError Message
    | UserFormError UserFormErrorData
    | ForbiddenError
    | SystemLogError SystemLogErrorData
    | TooManyRequestsError TooManyRequestsErrorData


type alias Message =
    { code : String
    , defaultMessage : String
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


type alias TooManyRequestsErrorData =
    { retryAfterSeconds : Int
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
        |> D.optional "defaultMessage" D.string ""
        |> D.required "params" (D.list D.string)


systemLogErrorDataDecoder : Decoder SystemLogErrorData
systemLogErrorDataDecoder =
    D.succeed SystemLogErrorData
        |> D.required "defaultMessage" D.string
        |> D.required "params" (D.list D.string)


tooManyRequestsErrorDecoder : Decoder ServerError
tooManyRequestsErrorDecoder =
    D.map TooManyRequestsError tooManyRequestsErrorDataDecoder


tooManyRequestsErrorDataDecoder : Decoder TooManyRequestsErrorData
tooManyRequestsErrorDataDecoder =
    D.succeed TooManyRequestsErrorData
        |> D.required "retryAfterSeconds" D.int


forbiddenMessage : { a | locale : Gettext.Locale } -> String
forbiddenMessage appState =
    gettext "You do not have permission to view this page." appState.locale


tooManyRequestsMessage : { a | locale : Gettext.Locale } -> TooManyRequestsErrorData -> String
tooManyRequestsMessage appState data =
    String.format
        (gettext "Too many requests. Try again after %s seconds." appState.locale)
        [ String.fromInt data.retryAfterSeconds ]


{-| The message as the user reads it: in their language when it is one we know,
in the server's own words when it is not, and nothing when the server sent no
words either.
-}
messageToText : { a | locale : Gettext.Locale } -> Message -> Maybe String
messageToText appState message =
    case messageToReadable appState message of
        Just readable ->
            Just readable

        Nothing ->
            if String.isEmpty (String.trim message.defaultMessage) then
                Nothing

            else
                Just (String.format message.defaultMessage message.params)


messageToReadable : { a | locale : Gettext.Locale } -> Message -> Maybe String
messageToReadable appState message =
    case message.code of
        "error.database.entity_not_found" ->
            case List.head message.params of
                Just "action_key" ->
                    Just <| gettext "Activation link is no longer valid." appState.locale

                Just "audit_log" ->
                    Just <| gettext "Audit log not found." appState.locale

                Just "automation" ->
                    Just <| gettext "Automation not found." appState.locale

                Just "automation_invocation" ->
                    Just <| gettext "Automation invocation not found." appState.locale

                Just "branch" ->
                    Just <| gettext "Knowledge model editor not found." appState.locale

                Just "document" ->
                    Just <| gettext "Document not found." appState.locale

                Just "document_template" ->
                    Just <| gettext "Document template not found." appState.locale

                Just "document_template_asset" ->
                    Just <| gettext "Document template asset not found." appState.locale

                Just "document_template_draft" ->
                    Just <| gettext "Document template draft not found." appState.locale

                Just "document_template_file" ->
                    Just <| gettext "Document template file not found." appState.locale

                Just "ih_value_integration" ->
                    Just <| gettext "Value integration not found." appState.locale

                Just "knowledge_model_migration" ->
                    Just <| gettext "Knowledge model migration not found." appState.locale

                Just "locale" ->
                    Just <| gettext "Locale not found." appState.locale

                Just "openid_client" ->
                    Just <| gettext "OpenID client not found." appState.locale

                Just "openid_provider" ->
                    Just <| gettext "OpenID provider not found." appState.locale

                Just "package" ->
                    Just <| gettext "Knowledge model not found." appState.locale

                Just "project" ->
                    Just <| gettext "Project not found." appState.locale

                Just "project_comment" ->
                    Just <| gettext "Comment not found." appState.locale

                Just "project_comment_thread" ->
                    Just <| gettext "Comment thread not found." appState.locale

                Just "project_event" ->
                    Just <| gettext "Project change not found." appState.locale

                Just "project_file" ->
                    Just <| gettext "Project file not found." appState.locale

                Just "project_version" ->
                    Just <| gettext "Project version not found." appState.locale

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

                Just "saml_service_provider" ->
                    Just <| gettext "SAML service provider not found." appState.locale

                Just "user_entity" ->
                    Just <| gettext "User not found." appState.locale

                Just "user_group" ->
                    Just <| gettext "User group not found." appState.locale

                _ ->
                    Just <| gettext "Not found." appState.locale

        -- Websocket
        "error.api.websocket.cant_deserialize_obj" ->
            Just <| gettext "The server could not read the change." appState.locale

        "error.api.websocket.queue_overflow" ->
            Just <| gettext "The connection is too slow to keep up with the changes." appState.locale

        "error.database.unique_constraint_violation" ->
            Just <| gettext "The change has already been saved." appState.locale

        "error.validation.km_id_uniqueness" ->
            Just <| gettext "Knowledge model ID is already used." appState.locale

        "error.validation.km_locale_invalid_po" ->
            Just <| String.format (gettext "Unable to parse the PO file: %s" appState.locale) message.params

        "error.validation.km_locale_missing_language" ->
            Just <| gettext "The PO file has no 'Language' header field." appState.locale

        "error.validation.km_locale_invalid_json" ->
            Just <| String.format (gettext "Unable to parse the JSON translation file: %s" appState.locale) message.params

        "error.validation.km_locale_code_uniqueness" ->
            Just <| String.format (gettext "Translation for language '%s' already exists." appState.locale) message.params

        "error.validation.km_locale_not_reusable" ->
            Just <| gettext "Selected locales cannot be reused from the previous package version." appState.locale

        "error.validation.logo_size_limit" ->
            Just <|
                case List.head message.params of
                    Just limitValue ->
                        String.format (gettext "Logo file size cannot exceed %s." appState.locale) [ limitValue ]

                    Nothing ->
                        gettext "Logo file size exceeds the limit." appState.locale

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

        -- Projects
        "error.validation.forbidden" ->
            Just <|
                case message.params of
                    [ "View Project" ] ->
                        gettext "You do not have permission to view this project." appState.locale

                    [ "Comment Project" ] ->
                        gettext "You do not have permission to comment on this project." appState.locale

                    [ "Edit Project" ] ->
                        gettext "You do not have permission to edit this project." appState.locale

                    [ "Administrate Project" ] ->
                        gettext "You do not have permission to manage this project." appState.locale

                    [ "Migrate Project" ] ->
                        gettext "You do not have permission to migrate this project." appState.locale

                    [ "Project Template" ] ->
                        gettext "This project is not a template." appState.locale

                    [ "Read Document" ] ->
                        gettext "You do not have permission to view this document." appState.locale

                    [ "Edit Document" ] ->
                        gettext "You do not have permission to edit this document." appState.locale

                    _ ->
                        gettext "You do not have permission to do this." appState.locale

        "error.service.common.feature_is_disabled" ->
            Just <|
                case message.params of
                    [ "Project Template" ] ->
                        gettext "Project templates are disabled." appState.locale

                    _ ->
                        gettext "This feature is disabled." appState.locale

        "error.service.workspace.not_member" ->
            Just <|
                case message.params of
                    [ email ] ->
                        String.format (gettext "User %s is not a member of the workspace." appState.locale) [ email ]

                    _ ->
                        gettext "The user is not a member of the workspace." appState.locale

        "error.validation.user_absence" ->
            Just <| String.format (gettext "User %s does not exist." appState.locale) message.params

        "error.validation.knowledge_model_package_absence" ->
            Just <| String.format (gettext "Knowledge model %s does not exist." appState.locale) message.params

        "error.validation.document_template_absence" ->
            Just <| String.format (gettext "Document template %s does not exist." appState.locale) message.params

        "error.validation.forbidden_characters" ->
            Just <| String.format (gettext "\"%s\" contains characters that are not allowed." appState.locale) message.params

        "error.validation.project_event_path_absence" ->
            Just <| gettext "The question no longer exists in the knowledge model of the project." appState.locale

        "error.validation.project_event_item_absence" ->
            Just <| gettext "The item no longer exists." appState.locale

        "error.validation.project_event_reply_type_mismatch" ->
            Just <| gettext "The answer does not match the type of the question." appState.locale

        "error.validation.project_event_answer_absence" ->
            Just <| gettext "The selected answer does not belong to the question." appState.locale

        "error.validation.project_event_choice_absence" ->
            Just <| gettext "A selected choice does not belong to the question." appState.locale

        "error.validation.project_event_selected_item_absence" ->
            Just <| gettext "The selected item no longer exists in the referenced list." appState.locale

        "error.validation.project_event_file_absence" ->
            Just <| gettext "The file does not belong to the project." appState.locale

        "error.validation.project_event_value_type" ->
            Just <| gettext "The value does not match the type of the question." appState.locale

        "error.validation.project_event_value_validation" ->
            Just <| gettext "The value does not pass the validation of the question." appState.locale

        "error.validation.project_file_question_absence_or_wrong_type" ->
            Just <| gettext "The question does not exist or is not a file question." appState.locale

        "error.validation.project_file_size_exceeds_limit" ->
            Just <|
                case List.map (Maybe.unwrap "0" ByteUnits.toReadable << String.toInt) message.params of
                    [ fileSize, maxFileSize ] ->
                        String.format (gettext "The file is too large (%s, the limit is %s)." appState.locale) [ fileSize, maxFileSize ]

                    _ ->
                        gettext "The file is too large." appState.locale

        "error.service.project.comment.not_author" ->
            Just <| gettext "Only the author of the comment can do this." appState.locale

        "error.service.project.comment.thread_resolved" ->
            Just <| gettext "The comment thread is resolved. Reopen it first." appState.locale

        "error.service.project.comment.assignee_without_access" ->
            Just <| gettext "The assigned user cannot comment on the project." appState.locale

        "error.service.project.version.uuid_taken" ->
            Just <| gettext "The version already exists." appState.locale

        "error.service.project.version.version_uniqueness" ->
            Just <| gettext "There is already a version for this change." appState.locale

        "error.service.project.version.non_existent_event_uuid" ->
            Just <| gettext "The version cannot be created for a change that does not exist." appState.locale

        "error.service.project.collaboration.force_disconnect" ->
            Just <| gettext "Another user has made significant changes to the project. Refresh the page to reconnect." appState.locale

        "error.service.openid.identity_linked_to_different_user" ->
            Just <| gettext "This account is already linked to a different user." appState.locale

        "error.service.openid.registration_disabled" ->
            Just <| gettext "Registration of new accounts via this service is disabled." appState.locale

        "error.service.pkg.highest_number_in_new_version" ->
            Just <| gettext "New version has to be higher than the previous one." appState.locale

        "error.service.tb.missing_template_json" ->
            Just <| gettext "\"template.json\" was not found in archive." appState.locale

        "error.service.tb.unable_to_decode_template_json" ->
            Just <| String.format (gettext "Error while parsing template.json: \"%s\"." appState.locale) message.params

        "error.service.tb.missing_asset" ->
            Just <| String.format (gettext "Asset \"%s\" was not found in archive." appState.locale) message.params

        "error.service.saml.registration_disabled" ->
            Just <| gettext "Registration of new accounts via this service is disabled." appState.locale

        -- Admin
        "error.validation.tenant_deal_id_uniqueness" ->
            Just "This deal was already added."

        -- Users
        "error.validation.email_uniqueness" ->
            Just <| gettext "Email is already used." appState.locale

        "error.validation.hash_absence" ->
            Just <| gettext "Link is not valid." appState.locale

        -- Wizard
        "error.validation.app_id_uniqueness" ->
            Just <| gettext "App ID is already used." appState.locale

        "error.validation.doc_tml_file_or_asset_uniqueness" ->
            Just <| gettext "File with this name already exists." appState.locale

        "error.validation.openid_code_absence" ->
            Just <| gettext "Authentication code is not provided." appState.locale

        "error.validation.openid_profile_info_absence" ->
            Just <| gettext "Profile Information from OpenID service is missing." appState.locale

        "error.service.library.delete_hidden_dependents" ->
            Just <| gettext "It cannot be deleted: it is also used where you have no access." appState.locale

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

        "error.service.user_email_link.expired" ->
            Just <| gettext "The link has expired." appState.locale

        "error.service.user.required_admin_role_or_hash_in_query_params" ->
            Just <| gettext "A hash query param has to be provided." appState.locale

        "error.service.user.required_hash_in_query_params" ->
            Just <| gettext "A hash query param has to be provided." appState.locale

        _ ->
            Nothing
