module Wizard.Utils.QuestionnaireUtils exposing
    ( QuestionnaireLike
    , canComment
    , isAnonymousProject
    , isEditor
    , isMigrating
    , isOwner
    )

import Common.Api.Models.UserInfo as UserInfo
import Flip exposing (flip)
import List.Extra as List
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Permission exposing (Permission)
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Wizard.Api.Models.QuestionnairePerm as QuestionnairePerm
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session


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


isEditor : AppState -> QuestionnaireLike q -> Bool
isEditor appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.edit


isOwner : AppState -> QuestionnaireLike q -> Bool
isOwner appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.admin


isAnonymousProject : QuestionnaireLike q -> Bool
isAnonymousProject questionnaire =
    List.isEmpty questionnaire.permissions


canComment : AppState -> QuestionnaireLike q -> Bool
canComment appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.comment && not (isMigrating questionnaire)


hasPerm : AppState -> QuestionnaireLike q -> String -> Bool
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
