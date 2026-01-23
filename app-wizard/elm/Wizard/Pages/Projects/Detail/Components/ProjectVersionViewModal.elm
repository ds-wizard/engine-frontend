module Wizard.Pages.Projects.Detail.Components.ProjectVersionViewModal exposing
    ( Model
    , Msg
    , ViewConfig
    , init
    , initEmpty
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FontAwesome exposing (faClose)
import Common.Components.Page as Page
import Common.Utils.TimeUtils as TimeUtils
import Html exposing (Html, button, div, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Shortcut
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.ProjectContent exposing (ProjectContent)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Api.Models.ProjectVersion as ProjectVersion exposing (ProjectVersion)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Questionnaire2 as Questionnaire
import Wizard.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Data.AppState exposing (AppState)



-- MODEL


type alias Model =
    { questionnaireModel : ActionResult Questionnaire.Model
    , projectContent : ActionResult ProjectContent
    , projectQuestionnaire : ActionResult ProjectQuestionnaire
    , projectCommon : ActionResult ProjectCommon
    , eventUuid : Maybe Uuid
    }


initEmpty : Model
initEmpty =
    { questionnaireModel = Unset
    , projectContent = Unset
    , projectQuestionnaire = Unset
    , projectCommon = Unset
    , eventUuid = Nothing
    }


init : AppState -> Uuid -> Uuid -> ( Model, Cmd Msg )
init appState projectUuid eventUuid =
    ( { questionnaireModel = Loading
      , projectContent = Loading
      , projectQuestionnaire = Loading
      , projectCommon = Loading
      , eventUuid = Just eventUuid
      }
    , Cmd.batch
        [ ProjectsApi.fetchPreview appState projectUuid eventUuid FetchPreviewComplete
        , ProjectsApi.getQuestionnaire appState projectUuid GetQuestionnaireComplete
        ]
    )



-- UPDATE


type Msg
    = FetchPreviewComplete (Result ApiError ProjectContent)
    | GetQuestionnaireComplete (Result ApiError (ProjectDetailWrapper ProjectQuestionnaire))
    | QuestionnaireMsg Questionnaire.Msg
    | Close


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    let
        initQuestionnaireModel ( m, cmd ) =
            let
                actionResult =
                    ActionResult.combine m.projectContent m.projectQuestionnaire
            in
            case actionResult of
                Success ( content, project ) ->
                    let
                        questionnaireModel =
                            ProjectQuestionnaire.updateContent project content
                                |> Questionnaire.initSimple appState
                                |> Tuple.first
                                |> Success
                    in
                    ( { m | questionnaireModel = questionnaireModel }, cmd )

                Error e ->
                    ( { m | questionnaireModel = Error e }, cmd )

                _ ->
                    ( m, cmd )
    in
    case msg of
        FetchPreviewComplete result ->
            initQuestionnaireModel <|
                case result of
                    Ok content ->
                        ( { model | projectContent = Success content }, Cmd.none )

                    Err error ->
                        ( { model | projectContent = ApiError.toActionResult appState "Unable to fetch questionnaire." error }
                        , Cmd.none
                        )

        GetQuestionnaireComplete result ->
            initQuestionnaireModel <|
                case result of
                    Ok questionnaire ->
                        ( { model | projectQuestionnaire = Success questionnaire.data }, Cmd.none )

                    Err error ->
                        ( { model | projectQuestionnaire = ApiError.toActionResult appState "Unable to fetch questionnaire." error }
                        , Cmd.none
                        )

        QuestionnaireMsg questionnaireMsg ->
            case ( model.projectCommon, model.questionnaireModel ) of
                ( Success questionnaireCommon, Success questionnaireModel ) ->
                    let
                        newQuestionnaireModel =
                            Questionnaire.update appState
                                { wrapMsg = QuestionnaireMsg
                                , mbSetFullScreenMsg = Nothing
                                , projectCommon = questionnaireCommon
                                }
                                questionnaireMsg
                                questionnaireModel
                    in
                    ( { model | questionnaireModel = Success newQuestionnaireModel.model }
                    , newQuestionnaireModel.cmd
                    )

                _ ->
                    ( model, Cmd.none )

        Close ->
            ( { model
                | questionnaireModel = Unset
                , projectContent = Unset
                , projectQuestionnaire = Unset
                , eventUuid = Nothing
              }
            , Cmd.none
            )



-- VIEW


type alias ViewConfig =
    { events : List ProjectEvent
    , versions : List ProjectVersion
    }


view : ViewConfig -> AppState -> Model -> Html Msg
view cfg appState model =
    let
        visible =
            not <| ActionResult.isUnset model.questionnaireModel

        datetime =
            cfg.events
                |> List.find (ProjectEvent.getUuid >> Just >> (==) model.eventUuid)
                |> Maybe.unwrap "" (ProjectEvent.getCreatedAt >> TimeUtils.toReadableDateTime appState.timeZone)

        versionBadge =
            model.eventUuid
                |> Maybe.andThen (ProjectVersion.getVersionByEventUuid cfg.versions)
                |> Maybe.unwrap Html.nothing QuestionnaireVersionTag.version

        shortcuts =
            if visible then
                [ Shortcut.simpleShortcut Shortcut.Escape Close ]

            else
                []
    in
    Shortcut.shortcutElement shortcuts
        [ class "ProjectVersionViewModal modal modal-cover", classList [ ( "visible", visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content", dataCy "modal_project-version" ]
                [ div [ class "modal-header" ]
                    [ strong [ class "modal-title" ] [ text datetime, versionBadge ]
                    , button [ class "close", onClick Close ]
                        [ faClose ]
                    ]
                , Page.actionResultView appState (viewContent appState) model.questionnaireModel
                ]
            ]
        ]


viewContent : AppState -> Questionnaire.Model -> Html Msg
viewContent appState qm =
    Questionnaire.view appState
        { wrapMsg = QuestionnaireMsg
        , readonly = True
        , toolbarEnabled = False
        , actionsEnabled = False
        , previewQuestionnaireEventMsg = Nothing
        , revertQuestionnaireMsg = Nothing
        }
        qm
