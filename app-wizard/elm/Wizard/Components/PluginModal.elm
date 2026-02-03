module Wizard.Components.PluginModal exposing
    ( Model
    , Msg
    , PluginModalState
    , ViewConfig
    , initialModel
    , open
    , update
    , view
    )

import Common.Components.Modal as Modal
import Html exposing (Html)
import Uuid exposing (Uuid)
import Wizard.Components.PluginView as PluginView
import Wizard.Data.AppState exposing (AppState)
import Wizard.Plugins.PluginElement as PluginElement exposing (PluginElement)


type alias Model a =
    { state : Maybe (PluginModalState a)
    }


type alias PluginModalState a =
    { pluginUuid : Uuid
    , pluginElement : PluginElement
    , data : a
    }


initialModel : Model a
initialModel =
    { state = Nothing }


type Msg a
    = Open (PluginModalState a)
    | Close


open : PluginModalState a -> Msg a
open =
    Open


update : Msg a -> Model a -> Model a
update msg model =
    case msg of
        Open state ->
            { model | state = Just state }

        Close ->
            { model | state = Nothing }


type alias ViewConfig a msg =
    { attributes : a -> List (Html.Attribute msg)
    , wrapMsg : Msg a -> msg
    }


view : AppState -> ViewConfig a msg -> Model a -> Html msg
view appState cfg model =
    let
        ( visible, content ) =
            case model.state of
                Just pluginModalState ->
                    ( True
                    , [ PluginView.view appState
                            pluginModalState.pluginUuid
                            pluginModalState.pluginElement
                            (PluginElement.onActionClose (cfg.wrapMsg Close)
                                :: cfg.attributes pluginModalState.data
                            )
                      ]
                    )

                Nothing ->
                    ( False, [] )
    in
    Modal.simple
        { modalContent = content
        , visible = visible
        , enterMsg = Nothing
        , escMsg = Just (cfg.wrapMsg Close)
        , dataCy = "plugin"
        }
