module Wizard.Pages.ProjectActions.Index.View exposing (view)

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faDisable, faEnable)
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Gettext exposing (gettext)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Wizard.Api.Models.ProjectAction exposing (ProjectAction)
import Wizard.Api.Models.ProjectImporter exposing (ProjectImporter)
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.ProjectActions.Index.Models exposing (Model)
import Wizard.Pages.ProjectActions.Index.Msgs exposing (Msg(..))
import Wizard.Pages.ProjectActions.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "ProjectActions__Index" ]
        [ Page.header (gettext "Project Actions" appState.locale) []
        , FormResult.view model.togglingEnabled
        , Listing.view appState (listingConfig appState) model.questionnaireActions
        ]


listingConfig : AppState -> ViewConfig ProjectAction Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = gettext "There are no project actions available." appState.locale
    , updated = Nothing
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search actions..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        ]
    , filters = []
    , toRoute = \_ -> Routes.ProjectActionsRoute << IndexRoute
    , toolbarExtra = Nothing
    }


listingTitle : AppState -> ProjectImporter -> Html Msg
listingTitle appState questionnaireImporter =
    span []
        [ text questionnaireImporter.name
        , listingTitleBadge appState questionnaireImporter
        ]


listingTitleBadge : AppState -> ProjectImporter -> Html Msg
listingTitleBadge appState questionnaireImporter =
    if questionnaireImporter.enabled then
        Badge.success [] [ text (gettext "enabled" appState.locale) ]

    else
        Badge.danger [] [ text (gettext "disabled" appState.locale) ]


listingDescription : ProjectImporter -> Html Msg
listingDescription questionnaireImporter =
    span []
        [ span [ class "fragment" ] [ text questionnaireImporter.description ]
        ]


listingActions : AppState -> ProjectImporter -> List (ListingDropdownItem Msg)
listingActions appState questionnaireImporter =
    let
        ( actionIcon, actionLabel ) =
            if questionnaireImporter.enabled then
                ( faDisable
                , gettext "Disable" appState.locale
                )

            else
                ( faEnable
                , gettext "Enable" appState.locale
                )

        toggleEnabledAction =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = actionIcon
                , label = actionLabel
                , msg = ListingActionMsg (ToggleEnabled questionnaireImporter)
                , dataCy = "toggle-enabled"
                }

        toggleEnabledVisible =
            Feature.projectImporters appState

        groups =
            [ [ ( toggleEnabledAction, toggleEnabledVisible ) ] ]
    in
    ListingDropdown.itemsFromGroups groups
