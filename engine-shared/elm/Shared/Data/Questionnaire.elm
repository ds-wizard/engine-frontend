module Shared.Data.Questionnaire exposing
    ( Questionnaire
    , decoder
    , getAnsweredIndication
    , isEditable
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Auth.Session as Session
import Shared.Data.PackageInfo as PackageInfo exposing (PackageInfo)
import Shared.Data.Permission as Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireReport as QuestionnaireReport exposing (QuestionnaireReport)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.SummaryReport as SummaryReport
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)
import Time
import Uuid exposing (Uuid)


type alias Questionnaire =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , isTemplate : Bool
    , package : PackageInfo
    , visibility : QuestionnaireVisibility
    , sharing : QuestionnaireSharing
    , permissions : List Permission
    , state : QuestionnaireState
    , updatedAt : Time.Posix
    , report : QuestionnaireReport
    }


isEditable : AbstractAppState a -> Questionnaire -> Bool
isEditable appState questionnaire =
    let
        isAdmin =
            UserInfo.isAdmin appState.session.user

        isReadonly =
            if questionnaire.sharing == AnyoneWithLinkEditQuestionnaire then
                False

            else if Session.exists appState.session then
                questionnaire.visibility == VisibleViewQuestionnaire || (questionnaire.visibility == PrivateQuestionnaire && not isOwner)

            else
                questionnaire.sharing == AnyoneWithLinkViewQuestionnaire

        isOwner =
            matchOwner questionnaire appState.session.user
    in
    isAdmin || not isReadonly || isOwner


decoder : Decoder Questionnaire
decoder =
    D.succeed Questionnaire
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "isTemplate" D.bool
        |> D.required "package" PackageInfo.decoder
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "state" QuestionnaireState.decoder
        |> D.required "updatedAt" D.datetime
        |> D.required "report" QuestionnaireReport.decoder


matchOwner : Questionnaire -> Maybe UserInfo -> Bool
matchOwner questionnaire mbUser =
    List.any (.member >> .uuid >> Just >> (==) (Maybe.map .uuid mbUser)) questionnaire.permissions


getAnsweredIndication : Questionnaire -> Maybe ( Int, Int )
getAnsweredIndication questionnaire =
    let
        toTuple answeredInidciation =
            let
                { answeredQuestions, unansweredQuestions } =
                    SummaryReport.unwrapIndicationReport answeredInidciation
            in
            ( answeredQuestions, unansweredQuestions )
    in
    questionnaire.report.indications
        |> List.sortWith SummaryReport.compareIndicationReport
        |> List.head
        |> Maybe.map toTuple
