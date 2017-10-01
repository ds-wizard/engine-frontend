module Main exposing (..)

import Html exposing (Html, div, program, text)


type alias Model =
    String


type Msg
    = NoOp


initialModel : Model
initialModel =
    "Hello"


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ text model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
