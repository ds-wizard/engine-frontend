module Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal exposing
    ( Model
    , Msg
    , ViewConfig
    , init
    , initEmpty
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, button, div, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Components.FontAwesome exposing (faClose)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.TimeUtils as TimeUtils
import Shortcut
import Triple
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireContent exposing (QuestionnaireContent)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnaireQuestionnaire as QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Api.Models.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Page as Page



-- MODEL


type alias Model =
    { questionnaireModel : ActionResult Questionnaire.Model
    , questionnaireContent : ActionResult QuestionnaireContent
    , questionnaireQuestionnaire : ActionResult QuestionnaireQuestionnaire
    , eventUuid : Maybe Uuid
    }


initEmpty : Model
initEmpty =
    { questionnaireModel = Unset
    , questionnaireContent = Unset
    , questionnaireQuestionnaire = Unset
    , eventUuid = Nothing
    }


init : AppState -> Uuid -> Uuid -> ( Model, Cmd Msg )
init appState questionnaireUuid eventUuid =
    ( { questionnaireModel = Loading
      , questionnaireContent = Loading
      , questionnaireQuestionnaire = Loading
      , eventUuid = Just eventUuid
      }
    , Cmd.batch
        [ QuestionnairesApi.fetchPreview appState questionnaireUuid eventUuid FetchPreviewComplete
        , QuestionnairesApi.getQuestionnaireQuestionnaire appState questionnaireUuid GetQuestionnaireComplete
        ]
    )



-- UPDATE


type Msg
    = FetchPreviewComplete (Result ApiError QuestionnaireContent)
    | GetQuestionnaireComplete (Result ApiError (QuestionnaireDetailWrapper QuestionnaireQuestionnaire))
    | QuestionnaireMsg Questionnaire.Msg
    | Close


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    let
        initQuestionnaireModel ( m, cmd ) =
            let
                actionResult =
                    ActionResult.combine m.questionnaireContent m.questionnaireQuestionnaire
            in
            case actionResult of
                Success ( content, questionnaire ) ->
                    let
                        questionnaireModel =
                            QuestionnaireQuestionnaire.updateContent questionnaire content
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
                        ( { model | questionnaireContent = Success content }, Cmd.none )

                    Err error ->
                        ( { model | questionnaireContent = ApiError.toActionResult appState "Unable to fetch questionnaire." error }
                        , Cmd.none
                        )

        GetQuestionnaireComplete result ->
            initQuestionnaireModel <|
                case result of
                    Ok questionnaire ->
                        ( { model | questionnaireQuestionnaire = Success questionnaire.data }, Cmd.none )

                    Err error ->
                        ( { model | questionnaireQuestionnaire = ApiError.toActionResult appState "Unable to fetch questionnaire." error }
                        , Cmd.none
                        )

        QuestionnaireMsg questionnaireMsg ->
            let
                updateQuestionnaire =
                    Triple.second
                        << Questionnaire.update
                            questionnaireMsg
                            QuestionnaireMsg
                            Nothing
                            appState
                            { events = []
                            , branchUuid = Nothing
                            }
            in
            ( { model | questionnaireModel = ActionResult.map updateQuestionnaire model.questionnaireModel }
            , Cmd.none
            )

        Close ->
            ( { model
                | questionnaireModel = Unset
                , questionnaireContent = Unset
                , questionnaireQuestionnaire = Unset
                , eventUuid = Nothing
              }
            , Cmd.none
            )



-- VIEW


type alias ViewConfig =
    { events : List QuestionnaireEvent
    , versions : List QuestionnaireVersion
    }


view : ViewConfig -> AppState -> Model -> Html Msg
view cfg appState model =
    let
        visible =
            not <| ActionResult.isUnset model.questionnaireModel

        datetime =
            cfg.events
                |> List.find (QuestionnaireEvent.getUuid >> Just >> (==) model.eventUuid)
                |> Maybe.unwrap "" (QuestionnaireEvent.getCreatedAt >> TimeUtils.toReadableDateTime appState.timeZone)

        versionBadge =
            model.eventUuid
                |> Maybe.andThen (QuestionnaireVersion.getVersionByEventUuid cfg.versions)
                |> Maybe.unwrap Html.nothing QuestionnaireVersionTag.version

        shortcuts =
            if visible then
                [ Shortcut.simpleShortcut Shortcut.Escape Close ]

            else
                []
    in
    Shortcut.shortcutElement shortcuts
        [ class "QuestionnaireVersionViewModal modal modal-cover", classList [ ( "visible", visible ) ] ]
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
        { features =
            { feedbackEnabled = False
            , todosEnabled = False
            , commentsEnabled = False
            , readonly = True
            , toolbarEnabled = False
            , questionLinksEnabled = False
            }
        , renderer = DefaultQuestionnaireRenderer.create appState qm.questionnaire.knowledgeModel (DefaultQuestionnaireRenderer.defaultResourcePageToRoute qm.questionnaire.packageId)
        , wrapMsg = QuestionnaireMsg
        , previewQuestionnaireEventMsg = Nothing
        , revertQuestionnaireMsg = Nothing
        , isKmEditor = False
        }
        { events = []
        , branchUuid = Nothing
        }
        qm
