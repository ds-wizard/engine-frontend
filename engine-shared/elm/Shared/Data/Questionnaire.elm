module Shared.Data.Questionnaire exposing
    ( Questionnaire
    , decoder
    , isEditable
    , isOwner
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import List.Extra as List
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Auth.Session as Session
import Shared.Data.Member as Member
import Shared.Data.PackageInfo as PackageInfo exposing (PackageInfo)
import Shared.Data.Permission as Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnairePerm as QuestionnairePerm
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)
import Shared.Utils exposing (flip)
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
    }


isEditable : AbstractAppState a -> Questionnaire -> Bool
isEditable appState questionnaire =
    let
        isAdmin =
            UserInfo.isAdmin appState.config.user

        isReadonly =
            if questionnaire.sharing == AnyoneWithLinkEditQuestionnaire then
                False

            else if Session.exists appState.session then
                questionnaire.visibility == VisibleViewQuestionnaire || (questionnaire.visibility == PrivateQuestionnaire && not isMember)

            else
                questionnaire.sharing == AnyoneWithLinkViewQuestionnaire

        isMember =
            matchMember questionnaire appState.config.user
    in
    isAdmin || not isReadonly || isMember


isOwner : AbstractAppState a -> Questionnaire -> Bool
isOwner appState questionnaire =
    let
        isAdmin =
            UserInfo.isAdmin appState.config.user

        isQuestionnaireOwner =
            appState.config.user
                |> Maybe.andThen (\user -> List.find (\p -> Member.getUuid p.member == user.uuid) questionnaire.permissions)
                |> Maybe.map (List.member QuestionnairePerm.admin << .perms)
                |> Maybe.withDefault False
    in
    isAdmin || isQuestionnaireOwner


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


matchMember : Questionnaire -> Maybe UserInfo -> Bool
matchMember questionnaire mbUser =
    case mbUser of
        Just user ->
            let
                userUuids =
                    user.uuid :: user.userGroupUuids

                memberUuids =
                    List.map (Member.getUuid << .member) questionnaire.permissions
            in
            List.any (flip List.member memberUuids) userUuids

        Nothing ->
            False
