module Shared.Error.ServerError exposing
    ( Message
    , ServerError(..)
    , decoder
    , forbiddenMessage
    , messageToReadable
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Locale exposing (lg, lgf)
import Shared.Provisioning exposing (Provisioning)


type ServerError
    = UserSimpleError Message
    | UserFormError UserFormErrorData
    | ForbiddenError


type alias Message =
    { code : String
    , params : List String
    }


type alias UserFormErrorData =
    { formErrors : List Message
    , fieldErrors : Dict String (List Message)
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


messageDecoder : Decoder Message
messageDecoder =
    D.succeed Message
        |> D.required "code" D.string
        |> D.required "params" (D.list D.string)


forbiddenMessage : { a | provisioning : Provisioning } -> String
forbiddenMessage appState =
    lg "apiError.forbidden" appState


messageToReadable : { a | provisioning : Provisioning } -> Message -> Maybe String
messageToReadable appState message =
    case message.code of
        -- Shared
        "error.validation.km_id_uniqueness" ->
            Just <| lg "apiError.validation.km_id_uniqueness" appState

        "error.validation.pkg_id_uniqueness" ->
            Just <| lg "apiError.validation.pkg_id_uniqueness" appState

        "error.validation.tml_id_uniqueness" ->
            Just <| lg "apiError.validation.tml_id_uniqueness" appState

        "error.validation.user_email_uniqueness" ->
            Just <| lg "apiError.validation.user_email_uniqueness" appState

        "error.service.pkg.highest_number_in_new_version" ->
            Just <| lg "apiError.service.pkg.highest_number_in_new_version" appState

        "error.service.tb.missing_template_json" ->
            Just <| lg "apiError.service.tb.missing_template_json" appState

        "error.service.tb.unable_to_decode_template_json" ->
            Just <| lgf "apiError.service.tb.unable_to_decode_template_json" message.params appState

        "error.service.tb.missing_asset" ->
            Just <| lgf "apiError.service.tb.missing_asset" message.params appState

        -- Registry
        "error.validation.email_uniqueness" ->
            Just <| lg "apiError.validation.email_uniqueness" appState

        "error.validation.hash_absence" ->
            Just <| lg "apiError.validation.hash_absence" appState

        "error.validation.organization_email_absence" ->
            Just <| lg "apiError.validation.organization_email_absence" appState

        "error.validation.organization_email_uniqueness" ->
            Just <| lg "apiError.validation.organization_email_uniqueness" appState

        "error.validation.organization_id_uniqueness" ->
            Just <| lg "apiError.validation.organization_id_uniqueness" appState

        "error.service.organization.required_hash_in_query_params" ->
            Just <| lg "apiError.service.organization.required_hash_in_query_params" appState

        -- Wizard
        "error.validation.openid_code_absence" ->
            Just <| lg "apiError.validation.openid_code_absence" appState

        "error.validation.openid_profile_info_absence" ->
            Just <| lg "apiError.validation.openid_profile_info_absence" appState

        "error.validation.tml_deletation" ->
            Just <| lg "apiError.validation.tml_deletation" appState

        "error.validation.tml_unsupported_version" ->
            Just <| lg "apiError.validation.tml_unsupported_version" appState

        "error.service.pkg.pkg_cant_be_deleted_because_it_is_used_by_some_other_entity" ->
            Just <| lg "apiError.service.pkg.pkg_cant_be_deleted_because_it_is_used_by_some_other_entity" appState

        "error.service.pb.pull_non_existing_pkg" ->
            Just <| lg "apiError.service.pb.pull_non_existing_pkg" appState

        "error.service.qtn.qtn_cant_be_deleted_because_it_is_used_in_migration" ->
            Just <| lg "apiError.service.qtn.qtn_cant_be_deleted_because_it_is_used_in_migration" appState

        "error.service.tb.pull_non_existing_tml" ->
            Just <| lg "apiError.service.tb.pull_non_existing_tml" appState

        "error.service.token.Incorrect_email_or_password" ->
            Just <| lg "apiError.service.token.Incorrect_email_or_password" appState

        "error.service.token.account_is_not_activated" ->
            Just <| lg "apiError.service.token.account_is_not_activated" appState

        "error.service.user.required_admin_role_or_hash_in_query_params" ->
            Just <| lg "apiError.service.user.required_admin_role_or_hash_in_query_params" appState

        "error.service.user.required_hash_in_query_params" ->
            Just <| lg "apiError.service.user.required_hash_in_query_params" appState

        _ ->
            Nothing
