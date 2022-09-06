module Wizard.ProjectImporters.Index.View exposing (view)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Shared.Components.Badge as Badge
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lg, lx)
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


l_ : String -> AppState -> String
l_ =
    l "Wizard.ProjectImporters.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.ProjectImporters.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "ProjectImporters__Index" ]
        [ Page.header (l_ "header.title" appState) []
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
    , emptyText = l_ "listing.empty" appState
    , updated = Nothing
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (l_ "listing.searchPlaceholderText" appState)
    , sortOptions =
        [ ( "name", lg "projectImporter.name" appState )
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
        Badge.success [] [ lx_ "badge.enabled" appState ]

    else
        Badge.danger [] [ lx_ "badge.disabled" appState ]


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
                , l_ "action.disable" appState
                )

            else
                ( faSet "questionnaireImporter.enable" appState
                , l_ "action.enable" appState
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
