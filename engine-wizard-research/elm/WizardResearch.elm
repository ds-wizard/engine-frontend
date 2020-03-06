module WizardResearch exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode exposing (Value)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l)
import Url exposing (Url)
import WizardResearch.Common.AppState as AppState exposing (AppState)


l_ : String -> AppState -> String
l_ =
    l "WizardResearch"


type alias Model =
    { appState : AppState }


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags location key =
    let
        appState =
            AppState.init flags key
    in
    ( { appState = appState }, Cmd.none )


view : Model -> Document Msg
view model =
    let
        body =
            div []
                [ h1 []
                    [ faSet "appIcon" model.appState
                    , text <| l_ "appName" model.appState
                    ]
                ]
    in
    { title = l_ "appName" model.appState
    , body = [ body ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
