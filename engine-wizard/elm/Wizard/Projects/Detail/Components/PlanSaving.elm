module Wizard.Projects.Detail.Components.PlanSaving exposing
    ( Model
    , Msg
    , init
    , setSaved
    , setSaving
    , subscriptions
    , update
    , view
    )

import Bootstrap.Popover as Popover
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (fa, faKeyClass)
import Shared.Locale exposing (l, lx)
import Time
import Wizard.Common.AppState exposing (AppState)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.PlanSaving"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.Components.PlanSaving"



-- MODEL


type alias Model =
    { state : State
    , popoverState : Popover.State
    }


type State
    = Saving
    | SavedRecently Int
    | Saved


init : Model
init =
    { state = Saved
    , popoverState = Popover.initialState
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
    = SavedTickMsg Time.Posix
    | PopoverMsg Popover.State


update : Msg -> Model -> Model
update msg model =
    case msg of
        SavedTickMsg _ ->
            case model.state of
                SavedRecently n ->
                    if n - 1 <= 0 then
                        { model | state = Saved }

                    else
                        { model | state = SavedRecently (n - 1) }

                _ ->
                    model

        PopoverMsg state ->
            { model | popoverState = state }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        SavedRecently _ ->
            Time.every (1 * 1000) SavedTickMsg

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
            viewSaved appState model


viewSaving : AppState -> Html Msg
viewSaving appState =
    viewHelper [] (faKeyClass "questionnaire.saving.saving" appState) (l_ "label.saving" appState)


viewSavedRecently : AppState -> Html Msg
viewSavedRecently appState =
    viewHelper [] (faKeyClass "questionnaire.saving.saved" appState) (l_ "label.saved" appState)


viewSaved : AppState -> Model -> Html Msg
viewSaved appState model =
    Popover.config
        (viewHelper (Popover.onHover model.popoverState PopoverMsg) (faKeyClass "questionnaire.saving.saved" appState) "")
        |> Popover.bottom
        |> Popover.content [] [ lx_ "popover.saved" appState ]
        |> Popover.view model.popoverState


viewHelper : List (Html.Attribute Msg) -> String -> String -> Html Msg
viewHelper attrs icon label =
    span (class "questionnaire-header__saving" :: attrs)
        [ fa icon
        , text label
        ]
