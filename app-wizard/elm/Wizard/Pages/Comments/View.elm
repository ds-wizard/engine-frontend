module Wizard.Pages.Comments.View exposing (view)

import Common.Components.Badge as Badge
import Common.Components.Page as Page
import Gettext exposing (gettext)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Wizard.Api.Models.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Api.Models.User as User
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Comments.Models exposing (Model)
import Wizard.Pages.Comments.Msgs exposing (Msg(..))
import Wizard.Routes as Routes exposing (commentsRouteResolvedFilterId)
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)


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
                Html.nothing
    in
    span []
        [ linkTo (Routes.projectsDetailQuestionnaire commentThread.questionnaireUuid (Just commentThread.path) (Just commentThread.commentThreadUuid))
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
                    Html.nothing
    in
    span []
        [ questionnaireNameFragment
        , userFragment
        ]
