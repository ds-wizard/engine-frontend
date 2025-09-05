module Wizard.Components.CopyableCodeBlock exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Common.Components.FontAwesome exposing (faCopy)
import Common.Components.Tooltip exposing (tooltip)
import Common.Ports.Copy as Copy
import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onMouseOut)
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { copied : Bool }


initialModel : Model
initialModel =
    { copied = False }


type Msg
    = CopyValue String
    | HideTooltip


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CopyValue value ->
            ( { model | copied = True }, Copy.copyToClipboard value )

        HideTooltip ->
            ( { model | copied = False }, Cmd.none )


view : AppState -> Model -> String -> Html Msg
view appState model value =
    let
        buttonTooltip =
            if model.copied then
                tooltip (gettext "Copied!" appState.locale)

            else
                []
    in
    div [ class "CopyableCodeBlock" ]
        [ a
            (class "btn btn-link with-icon"
                :: onClick (CopyValue value)
                :: onMouseOut HideTooltip
                :: buttonTooltip
            )
            [ faCopy
            , text (gettext "Copy" appState.locale)
            ]
        , code [] [ text value ]
        ]
