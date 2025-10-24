module Wizard.Pages.KMEditor.Common.DeleteModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Modal as Modal
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { kmEditor : Maybe ( Uuid, String )
    , deletingKmEditor : ActionResult String
    }


initialModel : Model
initialModel =
    { kmEditor = Nothing
    , deletingKmEditor = ActionResult.Unset
    }


type Msg
    = Open Uuid String
    | Delete
    | DeleteComplete (Result ApiError ())
    | Close


open : Uuid -> String -> Msg
open uuid name =
    Open uuid name


type alias UpdateConfig msg =
    { cmdDeleted : Cmd msg
    , wrapMsg : Msg -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Open uuid name ->
            ( { model | kmEditor = Just ( uuid, name ), deletingKmEditor = ActionResult.Unset }, Cmd.none )

        Delete ->
            case model.kmEditor of
                Just ( uuid, _ ) ->
                    ( { model | deletingKmEditor = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <| KnowledgeModelEditorsApi.deleteKnowledgeModelEditor appState uuid DeleteComplete
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteComplete result ->
            case result of
                Ok _ ->
                    ( { model | kmEditor = Nothing }, cfg.cmdDeleted )

                Err error ->
                    ( { model | deletingKmEditor = ApiError.toActionResult appState (gettext "Knowledge model could not be deleted." appState.locale) error }
                    , Cmd.none
                    )

        Close ->
            ( { model | kmEditor = Nothing }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, name ) =
            case model.kmEditor of
                Just ( _, kmEditorName ) ->
                    ( True, kmEditorName )

                Nothing ->
                    ( False, "" )

        content =
            [ p []
                (String.formatHtml (gettext "Are you sure you want to permanently delete %s?" appState.locale) [ strong [] [ text name ] ])
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete knowledge model editor" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingKmEditor
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) Delete
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "km-editor-delete"
    in
    Modal.confirm appState modalConfig
