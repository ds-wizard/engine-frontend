module Registry2.Pages.Signup exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Html exposing (Html)
import Registry2.Data.AppState exposing (AppState)


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type Msg
    = NoOp


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    ( model, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    Html.div [] [ Html.text "Signup" ]
