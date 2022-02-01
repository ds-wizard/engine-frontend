module Wizard.KMEditor.Common.DeleteModal exposing
    ( Model
    , Msg
    , initialModel
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Html exposing (Html, p, strong, text)
import Shared.Api.Branches as BranchesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lg, lh)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Common.DeleteModal"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KMEditor.Common.DeleteModal"


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
                    , Cmd.map cfg.wrapMsg <| BranchesApi.deleteBranch uuid appState DeleteComplete
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteComplete result ->
            case result of
                Ok _ ->
                    ( { model | branch = Nothing }, cfg.cmdDeleted )

                Err error ->
                    ( { model | deletingBranch = ApiError.toActionResult appState (lg "apiError.branches.deleteError" appState) error }
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
                (lh_ "text" [ strong [] [ text name ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "title" appState
            , modalContent = content
            , visible = visible
            , actionResult = model.deletingBranch
            , actionName = "Delete"
            , actionMsg = Delete
            , cancelMsg = Just Close
            , dangerous = True
            , dataCy = "km-editor-delete"
            }
    in
    Modal.confirm appState modalConfig
