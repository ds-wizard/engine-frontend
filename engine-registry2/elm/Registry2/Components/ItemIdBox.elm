module Registry2.Components.ItemIdBox exposing
    ( Msg
    , State
    , ViewProps
    , initialState
    , update
    , view
    )

import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, i, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick, onMouseOut)
import Registry2.Data.AppState exposing (AppState)
import Shared.Copy as Copy


type State
    = State StateData


type alias StateData =
    { copied : Bool }


initialState : State
initialState =
    State { copied = False }


type Msg
    = Copy String
    | Reset


update : Msg -> State -> ( State, Cmd Msg )
update msg (State state) =
    case msg of
        Copy value ->
            ( State { state | copied = True }
            , Copy.copyToClipboard value
            )

        Reset ->
            ( State { state | copied = False }, Cmd.none )


type alias ViewProps =
    { id : String
    }


view : AppState -> State -> ViewProps -> Html Msg
view appState (State state) props =
    let
        copyButtonLabel =
            if state.copied then
                gettext "Copied!" appState.locale

            else
                gettext "Click to copy" appState.locale
    in
    div [ class "item-id-box rounded border px-2 py-1" ]
        [ code [] [ text props.id ]
        , a
            [ class "copy-button with-tooltip with-tooltip-left text-muted"
            , attribute "data-tooltip" copyButtonLabel
            , onClick (Copy props.id)
            , onMouseOut Reset
            ]
            [ i [ class "far fa-copy" ] [] ]
        ]
