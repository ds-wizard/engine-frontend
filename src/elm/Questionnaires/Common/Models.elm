module Questionnaires.Common.Models exposing
    ( Questionnaire
    , isEditable
    , questionnaireDecoder
    , questionnaireListDecoder
    )

import Auth.Role as Role
import Common.AppState exposing (AppState)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)
import KnowledgeModels.Common.PackageDetail as PackageDetail exposing (PackageDetail)
import Questionnaires.Common.Models.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))


type alias Questionnaire =
    { uuid : String
    , name : String
    , package : PackageDetail
    , level : Int
    , accessibility : QuestionnaireAccessibility
    , ownerUuid : Maybe String
    }


isEditable :
    AppState
    ->
        { a
            | accessibility : QuestionnaireAccessibility
            , ownerUuid : Maybe String
        }
    -> Bool
isEditable appState questionnaire =
    let
        isAdmin =
            Role.isAdmin appState.session.user

        isNotReadonly =
            questionnaire.accessibility /= PublicReadOnlyQuestionnaire

        isOwner =
            questionnaire.ownerUuid == Maybe.map .uuid appState.session.user
    in
    isAdmin || isNotReadonly || isOwner


questionnaireDecoder : Decoder Questionnaire
questionnaireDecoder =
    Decode.succeed Questionnaire
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" PackageDetail.decoder
        |> optional "level" Decode.int 0
        |> required "accessibility" QuestionnaireAccessibility.decoder
        |> required "ownerUuid" (Decode.maybe Decode.string)


questionnaireListDecoder : Decoder (List Questionnaire)
questionnaireListDecoder =
    Decode.list questionnaireDecoder
