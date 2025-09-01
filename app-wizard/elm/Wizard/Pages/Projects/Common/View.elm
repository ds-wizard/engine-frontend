module Wizard.Pages.Projects.Common.View exposing
    ( QuestionnaireLike
    , shareIcon
    , shareTooltipHtml
    , visibilityIcon
    )

import Gettext exposing (gettext, ngettext)
import Html exposing (Html, small, span, strong, text)
import Html.Attributes exposing (class)
import Shared.Components.FontAwesome exposing (faProjectSharingInternal, faProjectSharingPrivate, faProjectSharingPublic)
import Shared.Components.Tooltip exposing (tooltipCustom)
import String.Format as String
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Permission exposing (Permission)
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Wizard.Data.AppState exposing (AppState)


visibilityIcon : AppState -> QuestionnaireLike q -> Html msg
visibilityIcon appState questionnaire =
    span (tooltipCustom "with-tooltip-wide with-tooltip-right" <| shareTooltipString appState questionnaire)
        [ shareIcon questionnaire ]


shareIcon : QuestionnaireLike q -> Html msg
shareIcon questionnaire =
    if isPublic questionnaire then
        faProjectSharingPublic

    else if isPrivate questionnaire then
        faProjectSharingPrivate

    else
        faProjectSharingInternal


shareTooltipHtml : AppState -> QuestionnaireLike q -> List (Html msg)
shareTooltipHtml appState questionnaire =
    if isPublic questionnaire then
        [ small [ class "d-block fw-bold" ] [ text <| gettext "Public link" appState.locale ]
        , small [] [ text <| gettext "Anyone with the link can access the project. No login is required." appState.locale ]
        ]

    else if isPrivate questionnaire then
        [ small [] [ text <| gettext "Private project accessible only by you." appState.locale ] ]

    else if questionnaire.visibility /= QuestionnaireVisibility.PrivateQuestionnaire then
        [ small [ class "d-block fw-bold" ] [ text <| gettext "Visible by all other logged-in users" appState.locale ]
        , small [] [ text <| gettext "Other logged-in users can access the project. No explicit permission is required." appState.locale ]
        ]

    else
        let
            ( userCount, userGroupCount ) =
                memberCounts questionnaire
        in
        if userCount > 0 && userGroupCount > 0 then
            [ small [] <|
                String.formatHtml (gettext "Shared with %s and %s." appState.locale)
                    [ span [] <|
                        String.formatHtml (ngettext ( "%s user", "%s users" ) userCount appState.locale)
                            [ strong [] [ text (String.fromInt userCount) ] ]
                    , span [] <|
                        String.formatHtml (ngettext ( "%s user group", "%s user groups" ) userGroupCount appState.locale)
                            [ strong [] [ text (String.fromInt userGroupCount) ] ]
                    ]
            ]

        else if userGroupCount > 0 then
            [ small [] <|
                String.formatHtml (ngettext ( "Shared with %s user group.", "Shared with %s user groups." ) userGroupCount appState.locale)
                    [ strong [] [ text (String.fromInt userGroupCount) ]
                    ]
            ]

        else
            [ small [] <|
                String.formatHtml (ngettext ( "Shared with %s user.", "Shared with %s users." ) userCount appState.locale)
                    [ strong [] [ text (String.fromInt userCount) ]
                    ]
            ]


shareTooltipString : AppState -> QuestionnaireLike q -> String
shareTooltipString appState questionnaire =
    if isPublic questionnaire then
        gettext "Anyone with the link can access the project." appState.locale

    else if isPrivate questionnaire then
        gettext "Private project accessible only by you." appState.locale

    else if questionnaire.visibility /= QuestionnaireVisibility.PrivateQuestionnaire then
        gettext "Other logged-in users can access the project." appState.locale

    else
        let
            ( userCount, userGroupCount ) =
                memberCounts questionnaire
        in
        if userCount > 0 && userGroupCount > 0 then
            String.format (gettext "Shared with %s and %s." appState.locale)
                [ String.format (ngettext ( "%s user", "%s users" ) userCount appState.locale) [ String.fromInt userCount ]
                , String.format (ngettext ( "%s user group", "%s user groups" ) userGroupCount appState.locale) [ String.fromInt userGroupCount ]
                ]

        else if userGroupCount > 0 then
            String.format (ngettext ( "Shared with %s user group.", "Shared with %s user groups." ) userGroupCount appState.locale)
                [ String.fromInt userGroupCount ]

        else
            String.format (ngettext ( "Shared with %s user.", "Shared with %s users." ) userCount appState.locale)
                [ String.fromInt userCount ]


type alias QuestionnaireLike q =
    { q
        | permissions : List Permission
        , sharing : QuestionnaireSharing
        , visibility : QuestionnaireVisibility
    }


isPrivate : QuestionnaireLike q -> Bool
isPrivate questionnaire =
    if questionnaire.sharing /= RestrictedQuestionnaire || questionnaire.visibility /= PrivateQuestionnaire then
        False

    else
        case questionnaire.permissions of
            perm :: [] ->
                Member.isUserMember perm.member

            _ ->
                False


isPublic : QuestionnaireLike q -> Bool
isPublic questionnaire =
    questionnaire.sharing /= RestrictedQuestionnaire


memberCounts : QuestionnaireLike q -> ( Int, Int )
memberCounts questionnaire =
    questionnaire.permissions
        |> List.map .member
        |> List.partition Member.isUserMember
        |> Tuple.mapBoth List.length List.length
