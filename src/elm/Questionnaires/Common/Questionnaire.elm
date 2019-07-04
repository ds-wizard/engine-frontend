module Questionnaires.Common.Questionnaire exposing
    ( Questionnaire
    , decoder
    , isEditable
    )

import Auth.Role as Role
import Common.AppState exposing (AppState)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)
import KnowledgeModels.Common.Package as Package exposing (Package)
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))
import Questionnaires.Common.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)


type alias Questionnaire =
    { uuid : String
    , name : String
    , package : Package
    , level : Int
    , accessibility : QuestionnaireAccessibility
    , ownerUuid : Maybe String
    , state : QuestionnaireState
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


decoder : Decoder Questionnaire
decoder =
    Decode.succeed Questionnaire
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" Package.decoder
        |> optional "level" Decode.int 0
        |> required "accessibility" QuestionnaireAccessibility.decoder
        |> required "ownerUuid" (Decode.maybe Decode.string)
        |> required "state" QuestionnaireState.decoder
