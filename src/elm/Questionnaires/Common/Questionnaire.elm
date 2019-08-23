module Questionnaires.Common.Questionnaire exposing
    ( Questionnaire
    , decoder
    , isEditable
    )

import Common.AppState exposing (AppState)
import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline exposing (optional, required)
import KnowledgeModels.Common.Package as Package exposing (Package)
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))
import Questionnaires.Common.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)
import Time
import Users.Common.User as User


type alias Questionnaire =
    { uuid : String
    , name : String
    , package : Package
    , level : Int
    , accessibility : QuestionnaireAccessibility
    , ownerUuid : Maybe String
    , state : QuestionnaireState
    , updatedAt : Time.Posix
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
            User.isAdmin appState.session.user

        isNotReadonly =
            questionnaire.accessibility /= PublicReadOnlyQuestionnaire

        isOwner =
            questionnaire.ownerUuid == Maybe.map .uuid appState.session.user
    in
    isAdmin || isNotReadonly || isOwner


decoder : Decoder Questionnaire
decoder =
    D.succeed Questionnaire
        |> required "uuid" D.string
        |> required "name" D.string
        |> required "package" Package.decoder
        |> optional "level" D.int 0
        |> required "accessibility" QuestionnaireAccessibility.decoder
        |> required "ownerUuid" (D.maybe D.string)
        |> required "state" QuestionnaireState.decoder
        |> required "updatedAt" D.datetime
