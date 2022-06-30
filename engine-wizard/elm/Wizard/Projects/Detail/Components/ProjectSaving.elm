module Wizard.Projects.Detail.Components.ProjectSaving exposing
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

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (fa, faKeyClass)
import Shared.Locale exposing (l)
import Time
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (tooltipRight)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.ProjectSaving"



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
    viewHelper [] (faKeyClass "questionnaire.saving.saving" appState) (l_ "label.saving" appState)


viewSavedRecently : AppState -> Html Msg
viewSavedRecently appState =
    viewHelper [] (faKeyClass "questionnaire.saving.saved" appState) (l_ "label.saved" appState)


viewSaved : AppState -> Html Msg
viewSaved appState =
    viewHelper (tooltipRight (l_ "popover.saved" appState)) (faKeyClass "questionnaire.saving.saved" appState) ""


viewHelper : List (Html.Attribute Msg) -> String -> String -> Html Msg
viewHelper attrs icon label =
    span (class "questionnaire-header__saving" :: attrs)
        [ fa icon
        , text label
        ]
