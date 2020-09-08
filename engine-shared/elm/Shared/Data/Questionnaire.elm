module Shared.Data.Questionnaire exposing
    ( Questionnaire
    , compare
    , decoder
    , isEditable
    )

import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Auth.Session as Session
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.Questionnaire.QuestionnaireReport as QuestionnaireReport exposing (QuestionnaireReport)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
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
    , sharing : QuestionnaireSharing
    , owner : Maybe User
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
            Maybe.isJust questionnaire.owner && Maybe.map .uuid questionnaire.owner == Maybe.map .uuid appState.session.user
    in
    isAdmin || not isReadonly || isOwner


decoder : Decoder Questionnaire
decoder =
    D.succeed Questionnaire
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "package" Package.decoder
        |> D.optional "level" D.int 0
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "owner" (D.maybe User.decoder)
        |> D.required "state" QuestionnaireState.decoder
        |> D.required "updatedAt" D.datetime
        |> D.required "report" QuestionnaireReport.decoder


compare : Questionnaire -> Questionnaire -> Order
compare q1 q2 =
    Basics.compare (String.toLower q1.name) (String.toLower q2.name)


matchOwner : Questionnaire -> Maybe UserInfo -> Bool
matchOwner questionnaire mbUser =
    Maybe.map .uuid questionnaire.owner == Maybe.map .uuid mbUser
