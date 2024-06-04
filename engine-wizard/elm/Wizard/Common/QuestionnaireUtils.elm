module Wizard.Common.QuestionnaireUtils exposing
    ( QuestionnaireLike
    , canComment
    , isAnonymousProject
    , isEditor
    , isMigrating
    , isOwner
    )

import List.Extra as List
import Maybe.Extra as Maybe
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Auth.Session as Session
import Shared.Data.Member as Member
import Shared.Data.Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnairePerm as QuestionnairePerm
import Shared.Data.UserInfo as UserInfo
import Shared.Utils exposing (flip)
import Uuid exposing (Uuid)


type alias QuestionnaireLike q =
    { q
        | permissions : List Permission
        , sharing : QuestionnaireSharing
        , visibility : QuestionnaireVisibility
        , migrationUuid : Maybe Uuid
    }


isMigrating : { q | migrationUuid : Maybe Uuid } -> Bool
isMigrating =
    Maybe.isJust << .migrationUuid


isEditor : AbstractAppState a -> QuestionnaireLike q -> Bool
isEditor appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.edit


isOwner : AbstractAppState a -> QuestionnaireLike q -> Bool
isOwner appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.admin


isAnonymousProject : QuestionnaireLike q -> Bool
isAnonymousProject questionnaire =
    List.isEmpty questionnaire.permissions


canComment : AbstractAppState a -> QuestionnaireLike q -> Bool
canComment appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.comment && not (isMigrating questionnaire)


hasPerm : AbstractAppState a -> QuestionnaireLike q -> String -> Bool
hasPerm appState questionnaire role =
    let
        mbUser =
            appState.config.user

        isAuthenticated =
            Session.exists appState.session

        globalPerms =
            if UserInfo.isAdmin mbUser then
                QuestionnairePerm.all

            else
                []

        visibilityPerms =
            if isAuthenticated then
                case questionnaire.visibility of
                    VisibleEditQuestionnaire ->
                        [ QuestionnairePerm.view, QuestionnairePerm.comment, QuestionnairePerm.edit ]

                    VisibleCommentQuestionnaire ->
                        [ QuestionnairePerm.view, QuestionnairePerm.comment ]

                    VisibleViewQuestionnaire ->
                        [ QuestionnairePerm.view ]

                    PrivateQuestionnaire ->
                        []

            else
                []

        sharingPerms =
            case questionnaire.sharing of
                AnyoneWithLinkEditQuestionnaire ->
                    [ QuestionnairePerm.view, QuestionnairePerm.comment, QuestionnairePerm.edit ]

                AnyoneWithLinkCommentQuestionnaire ->
                    [ QuestionnairePerm.view, QuestionnairePerm.comment ]

                AnyoneWithLinkViewQuestionnaire ->
                    [ QuestionnairePerm.view ]

                RestrictedQuestionnaire ->
                    []

        memberPerms =
            case mbUser of
                Just user ->
                    let
                        userUuids =
                            user.uuid :: user.userGroupUuids
                    in
                    questionnaire.permissions
                        |> List.filter (flip List.member userUuids << Member.getUuid << .member)
                        |> List.concatMap .perms

                Nothing ->
                    []

        appliedPerms =
            List.unique <| globalPerms ++ visibilityPerms ++ sharingPerms ++ memberPerms
    in
    List.member role appliedPerms
