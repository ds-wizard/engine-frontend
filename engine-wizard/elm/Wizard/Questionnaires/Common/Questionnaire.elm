module Wizard.Questionnaires.Common.Questionnaire exposing
    ( Questionnaire
    , compare
    , decoder
    , isEditable
    )

import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline exposing (optional, required)
import Time
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.UserInfo as UserInfo exposing (UserInfo)
import Wizard.KnowledgeModels.Common.Package as Package exposing (Package)
import Wizard.Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))
import Wizard.Questionnaires.Common.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)
import Wizard.Users.Common.User as User exposing (User)


type alias Questionnaire =
    { uuid : String
    , name : String
    , package : Package
    , level : Int
    , accessibility : QuestionnaireAccessibility
    , owner : Maybe User
    , state : QuestionnaireState
    , updatedAt : Time.Posix
    }


isEditable : AppState -> Questionnaire -> Bool
isEditable appState questionnaire =
    let
        isAdmin =
            UserInfo.isAdmin appState.session.user

        isNotReadonly =
            questionnaire.accessibility /= PublicReadOnlyQuestionnaire

        isOwner =
            matchOwner questionnaire appState.session.user
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
        |> required "owner" (D.maybe User.decoder)
        |> required "state" QuestionnaireState.decoder
        |> required "updatedAt" D.datetime


compare : Questionnaire -> Questionnaire -> Order
compare q1 q2 =
    Basics.compare (String.toLower q1.name) (String.toLower q2.name)


matchOwner : Questionnaire -> Maybe UserInfo -> Bool
matchOwner questionnaire mbUser =
    Maybe.map .uuid questionnaire.owner == Maybe.map .uuid mbUser
