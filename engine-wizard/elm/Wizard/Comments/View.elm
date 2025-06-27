module Wizard.Comments.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Shared.Components.Badge as Badge
import Shared.Html exposing (emptyNode)
import Wizard.Api.Models.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Api.Models.User as User
import Wizard.Comments.Models exposing (Model)
import Wizard.Comments.Msgs exposing (Msg(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.Page as Page
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Routes as Routes exposing (commentsRouteResolvedFilterId)


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "Comments__Index" ]
        [ Page.header (gettext "Assigned Comments" appState.locale) []
        , Listing.view appState (listingConfig appState) model.commentThreads
        ]


listingConfig : AppState -> ViewConfig QuestionnaireCommentThreadAssigned Msg
listingConfig appState =
    let
        resolvedFilter =
            Listing.SimpleFilter commentsRouteResolvedFilterId
                { name = gettext "Resolved" appState.locale
                , options =
                    [ ( "true", gettext "Resolved only" appState.locale )
                    , ( "false", gettext "Unresolved only" appState.locale )
                    ]
                }
    in
    { title = listingTitle appState
    , description = listingDescription
    , itemAdditionalData = always Nothing
    , dropdownItems = always []
    , textTitle = .questionnaireName
    , emptyText = gettext "No comments have been assigned to you." appState.locale
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search comments..." appState.locale)
    , sortOptions =
        [ ( "questionnaireUuid", gettext "Project" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
        ]
    , filters = [ resolvedFilter ]
    , toRoute = Routes.commentsIndexWithFilters
    , toolbarExtra = Nothing
    }


listingTitle : AppState -> QuestionnaireCommentThreadAssigned -> Html Msg
listingTitle appState commentThread =
    let
        resolvedBadge =
            if commentThread.resolved then
                Badge.success [] [ text (gettext "Resolved" appState.locale) ]

            else
                emptyNode
    in
    span []
        [ linkTo appState
            (Routes.projectsDetailQuestionnaire commentThread.questionnaireUuid (Just commentThread.path) (Just commentThread.commentThreadUuid))
            []
            [ text commentThread.text ]
        , resolvedBadge
        ]


listingDescription : QuestionnaireCommentThreadAssigned -> Html Msg
listingDescription commentThread =
    let
        questionnaireNameFragment =
            span [ class "fragment" ] [ text commentThread.questionnaireName ]

        userFragment =
            case commentThread.createdBy of
                Just createdBy ->
                    span [ class "fragment" ]
                        [ UserIcon.viewSmall createdBy
                        , text (User.fullName createdBy)
                        ]

                _ ->
                    emptyNode
    in
    span []
        [ questionnaireNameFragment
        , userFragment
        ]
