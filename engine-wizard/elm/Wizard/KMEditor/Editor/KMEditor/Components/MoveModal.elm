module Wizard.KMEditor.Editor.KMEditor.Components.MoveModal exposing
    ( Model
    , Msg(..)
    , ViewProps
    , close
    , getSelectedTargetUuid
    , initialModel
    , open
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Shared.Locale exposing (lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Modal as Modal
import Wizard.KMEditor.Editor.KMEditor.Components.MoveModalTreeInput as MoveModalTreeInput
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..))


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.KMEditor.Components.MoveModal"


type Msg
    = Submit
    | Close
    | MoveModalTreeInputMsg MoveModalTreeInput.Msg


type alias Model =
    { visible : Bool
    , treeInputModel : MoveModalTreeInput.Model
    }


initialModel : String -> Model
initialModel kmUuid =
    { visible = False
    , treeInputModel = MoveModalTreeInput.initialModel kmUuid
    }


getSelectedTargetUuid : Model -> String
getSelectedTargetUuid =
    .treeInputModel >> .uuid


close : Model -> Model
close model =
    { model | visible = False }


open : Model -> Model
open model =
    { model | visible = True }


type alias UpdateProps =
    { editors : Dict String Editor }


update : Msg -> Model -> UpdateProps -> Model
update msg model props =
    case msg of
        Close ->
            close model

        MoveModalTreeInputMsg moveModalTreeInputMsg ->
            { model | treeInputModel = MoveModalTreeInput.update moveModalTreeInputMsg model.treeInputModel props }

        _ ->
            model


type alias ViewProps =
    { editors : Dict String Editor
    , kmUuid : String
    , kmName : String
    , movingUuid : String
    }


view : AppState -> ViewProps -> Model -> Html Msg
view appState viewProps model =
    let
        treeInput =
            Html.map MoveModalTreeInputMsg <|
                MoveModalTreeInput.view appState model.treeInputModel viewProps

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ lx_ "view.modalTitle" appState ]
                ]
            , div [ class "modal-body" ]
                [ label [] [ lx_ "view.selectParent" appState ]
                , treeInput
                ]
            , div [ class "modal-footer" ]
                [ button
                    [ class "btn btn-primary"
                    , onClick Submit
                    , disabled (String.isEmpty <| getSelectedTargetUuid model)
                    , dataCy "modal_action-button"
                    ]
                    [ lx_ "view.move" appState ]
                , button
                    [ class "btn btn-secondary"
                    , onClick Close
                    , dataCy "modal_cancel-button"
                    ]
                    [ lx_ "view.cancel" appState ]
                ]
            ]

        modalConfig =
            { modalContent = content
            , visible = model.visible
            , dataCy = "km-editor-move"
            }
    in
    Modal.simple modalConfig
