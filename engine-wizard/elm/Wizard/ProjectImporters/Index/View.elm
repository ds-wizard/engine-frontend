module Wizard.ProjectImporters.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Shared.Components.Badge as Badge
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Html exposing (faSet)
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.ProjectImporters.Index.Models exposing (Model)
import Wizard.ProjectImporters.Index.Msgs exposing (Msg(..))
import Wizard.ProjectImporters.Routes exposing (Route(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "ProjectImporters__Index" ]
        [ Page.header (gettext "Project Importers" appState.locale) []
        , FormResult.view appState model.togglingEnabled
        , Listing.view appState (listingConfig appState) model.questionnaireImporters
        ]


listingConfig : AppState -> ViewConfig QuestionnaireImporter Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = gettext "There are no project importers available." appState.locale
    , updated = Nothing
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search importers..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        ]
    , filters = []
    , toRoute = \_ -> Routes.ProjectImportersRoute << IndexRoute
    , toolbarExtra = Nothing
    }


listingTitle : AppState -> QuestionnaireImporter -> Html Msg
listingTitle appState questionnaireImporter =
    span []
        [ text questionnaireImporter.name
        , listingTitleBadge appState questionnaireImporter
        ]


listingTitleBadge : AppState -> QuestionnaireImporter -> Html Msg
listingTitleBadge appState questionnaireImporter =
    if questionnaireImporter.enabled then
        Badge.success [] [ text (gettext "enabled" appState.locale) ]

    else
        Badge.danger [] [ text (gettext "disabled" appState.locale) ]


listingDescription : QuestionnaireImporter -> Html Msg
listingDescription questionnaireImporter =
    span []
        [ span [ class "fragment" ] [ text questionnaireImporter.description ]
        ]


listingActions : AppState -> QuestionnaireImporter -> List (ListingDropdownItem Msg)
listingActions appState questionnaireImporter =
    let
        ( actionIcon, actionLabel ) =
            if questionnaireImporter.enabled then
                ( faSet "questionnaireImporter.disable" appState
                , gettext "Disable" appState.locale
                )

            else
                ( faSet "questionnaireImporter.enable" appState
                , gettext "Enable" appState.locale
                )

        toggleEnabledAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = actionIcon
                , label = actionLabel
                , msg = ListingActionMsg (ToggleEnabled questionnaireImporter)
                , dataCy = "toggle-enabled"
                }

        toggleEnabledVisible =
            Feature.projectImporters appState
    in
    []
        |> listInsertIf toggleEnabledAction toggleEnabledVisible
