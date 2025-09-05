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
import Common.Components.Modal as Modal
import Common.Data.ApiError as ApiError exposing (ApiError)
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.Branches as BranchesApi
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { branch : Maybe ( Uuid, String )
    , deletingBranch : ActionResult String
    }


initialModel : Model
initialModel =
    { branch = Nothing
    , deletingBranch = ActionResult.Unset
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
            ( { model | branch = Just ( uuid, name ), deletingBranch = ActionResult.Unset }, Cmd.none )

        Delete ->
            case model.branch of
                Just ( uuid, _ ) ->
                    ( { model | deletingBranch = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <| BranchesApi.deleteBranch appState uuid DeleteComplete
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteComplete result ->
            case result of
                Ok _ ->
                    ( { model | branch = Nothing }, cfg.cmdDeleted )

                Err error ->
                    ( { model | deletingBranch = ApiError.toActionResult appState (gettext "Knowledge model could not be deleted." appState.locale) error }
                    , Cmd.none
                    )

        Close ->
            ( { model | branch = Nothing }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, name ) =
            case model.branch of
                Just ( _, branchName ) ->
                    ( True, branchName )

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
                |> Modal.confirmConfigActionResult model.deletingBranch
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) Delete
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "km-editor-delete"
    in
    Modal.confirm appState modalConfig
