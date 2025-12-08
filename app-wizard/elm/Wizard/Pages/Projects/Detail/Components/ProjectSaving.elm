module Wizard.Pages.Projects.Detail.Components.ProjectSaving exposing
    ( Model
    , Msg
    , State
    , init
    , setSaved
    , setSaving
    , subscriptions
    , update
    , view
    )

import Common.Components.FontAwesome exposing (faQuestionnaireSavingSaved, faQuestionnaireSavingSaving)
import Common.Components.Tooltip exposing (tooltipRight)
import Gettext exposing (gettext)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Time
import Wizard.Data.AppState exposing (AppState)



-- MODEL


type alias Model =
    { state : State
    }


type State
    = Saving
    | SavedRecently Int
    | Saved


init : Model
init =
    { state = Saved
    }


setSaving : Model -> Model
setSaving model =
    { model | state = Saving }


savedFullTextVisibleFor : Int
savedFullTextVisibleFor =
    3


setSaved : Model -> Model
setSaved model =
    { model | state = SavedRecently savedFullTextVisibleFor }



-- UPDATE


type Msg
    = SavedTickMsg


update : Msg -> Model -> Model
update _ model =
    case model.state of
        SavedRecently n ->
            if n - 1 <= 0 then
                { model | state = Saved }

            else
                { model | state = SavedRecently (n - 1) }

        _ ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        SavedRecently _ ->
            Time.every 1000 (always SavedTickMsg)

        _ ->
            Sub.none



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    case model.state of
        Saving ->
            viewSaving appState

        SavedRecently _ ->
            viewSavedRecently appState

        Saved ->
            viewSaved appState


viewSaving : AppState -> Html Msg
viewSaving appState =
    viewHelper [] faQuestionnaireSavingSaving (gettext "Saving..." appState.locale)


viewSavedRecently : AppState -> Html Msg
viewSavedRecently appState =
    viewHelper [] faQuestionnaireSavingSaved (gettext "Saved" appState.locale)


viewSaved : AppState -> Html Msg
viewSaved appState =
    viewHelper (tooltipRight (gettext "All changes have been saved" appState.locale)) faQuestionnaireSavingSaved ""


viewHelper : List (Html.Attribute Msg) -> Html Msg -> String -> Html Msg
viewHelper attrs icon label =
    span (class "project-header__saving" :: attrs)
        [ icon
        , text label
        ]
