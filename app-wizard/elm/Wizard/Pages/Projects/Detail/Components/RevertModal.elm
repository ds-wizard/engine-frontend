module Wizard.Pages.Projects.Detail.Components.RevertModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , setEvent
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Flash as Flash
import Common.Components.Modal as Modal
import Common.Ports.Window as Window
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html, br, p, strong, text)
import Maybe.Extra as Maybe
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Data.AppState exposing (AppState)



-- MODEL


type alias Model =
    { mbEvent : Maybe ProjectEvent
    , revertResult : ActionResult ()
    }


init : Model
init =
    { mbEvent = Nothing
    , revertResult = Unset
    }


setEvent : ProjectEvent -> Model -> Model
setEvent event model =
    { model
        | mbEvent = Just event
        , revertResult = Unset
    }



-- UPDATE


type Msg
    = Revert
    | PostRevertVersionComplete (Result ApiError ())
    | Close


type alias UpdateConfig =
    { projectUuid : Uuid
    }


update : UpdateConfig -> AppState -> Msg -> Model -> ( Model, Cmd Msg )
update cfg appState msg model =
    case msg of
        Revert ->
            case model.mbEvent of
                Just event ->
                    let
                        cmd =
                            ProjectsApi.postRevert appState cfg.projectUuid (ProjectEvent.getUuid event) PostRevertVersionComplete
                    in
                    ( { model | revertResult = Loading }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        PostRevertVersionComplete result ->
            case result of
                Ok _ ->
                    ( model, Window.refresh () )

                Err error ->
                    ( { model | revertResult = ApiError.toActionResult appState "Unable to revert the project" error }
                    , Cmd.none
                    )

        Close ->
            ( { model | mbEvent = Nothing }, Cmd.none )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        datetime =
            Maybe.unwrap "" (ProjectEvent.getCreatedAt >> TimeUtils.toReadableDateTime appState.timeZone) model.mbEvent

        content =
            [ Flash.warning (gettext "Heads up! This action cannot be undone." appState.locale)
            , p []
                (String.formatHtml
                    (gettext "Are you sure you want to revert the projects to its state from %s?" appState.locale)
                    [ strong [] [ br [] [], text datetime ] ]
                )
            ]

        cfg =
            Modal.confirmConfig (gettext "Revert questionnaire" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible (Maybe.isJust model.mbEvent)
                |> Modal.confirmConfigActionResult (ActionResult.map (always "") model.revertResult)
                |> Modal.confirmConfigAction (gettext "Revert" appState.locale) Revert
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "project-revert"
    in
    Modal.confirm appState cfg
