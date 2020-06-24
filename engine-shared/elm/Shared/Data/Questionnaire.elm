module Shared.Data.Questionnaire exposing
    ( Questionnaire
    , compare
    , decoder
    , isEditable
    )

import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline exposing (optional, required)
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.SummaryReport exposing (IndicationReport, indicationReportDecoder)
import Shared.Data.User as User exposing (User)
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)
import Time
import Uuid exposing (Uuid)


type alias Questionnaire =
    { uuid : Uuid
    , name : String
    , package : Package
    , level : Int
    , visibility : QuestionnaireVisibility
    , owner : Maybe User
    , state : QuestionnaireState
    , updatedAt : Time.Posix
    , report : Report
    }


type alias Report =
    { indications : List IndicationReport
    }


isEditable : AbstractAppState a -> Questionnaire -> Bool
isEditable appState questionnaire =
    let
        isAdmin =
            UserInfo.isAdmin appState.session.user

        isNotReadonly =
            questionnaire.visibility /= PublicReadOnlyQuestionnaire

        isOwner =
            matchOwner questionnaire appState.session.user
    in
    isAdmin || isNotReadonly || isOwner


decoder : Decoder Questionnaire
decoder =
    D.succeed Questionnaire
        |> required "uuid" Uuid.decoder
        |> required "name" D.string
        |> required "package" Package.decoder
        |> optional "level" D.int 0
        |> required "visibility" QuestionnaireVisibility.decoder
        |> required "owner" (D.maybe User.decoder)
        |> required "state" QuestionnaireState.decoder
        |> required "updatedAt" D.datetime
        |> required "report" reportDecoder


reportDecoder : Decoder { indications : List IndicationReport }
reportDecoder =
    D.succeed Report
        |> required "indications" (D.list indicationReportDecoder)


compare : Questionnaire -> Questionnaire -> Order
compare q1 q2 =
    Basics.compare (String.toLower q1.name) (String.toLower q2.name)


matchOwner : Questionnaire -> Maybe UserInfo -> Bool
matchOwner questionnaire mbUser =
    Maybe.map .uuid questionnaire.owner == Maybe.map .uuid mbUser
