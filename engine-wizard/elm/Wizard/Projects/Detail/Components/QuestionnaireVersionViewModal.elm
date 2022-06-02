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
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.QuestionnaireContent exposing (QuestionnaireContent)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode, faSet)
import Triple
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Page as Page



-- MODEL


type alias Model =
    { questionnaireModel : ActionResult Questionnaire.Model
    , eventUuid : Maybe Uuid
    }


initEmpty : Model
initEmpty =
    { questionnaireModel = Unset
    , eventUuid = Nothing
    }


init : AppState -> Uuid -> Uuid -> ( Model, Cmd Msg )
init appState quetionnaireUuid eventUuid =
    ( { questionnaireModel = Loading, eventUuid = Just eventUuid }
    , QuestionnairesApi.fetchPreview quetionnaireUuid eventUuid appState FetchPreviewComplete
    )



-- UPDATE


type Msg
    = FetchPreviewComplete (Result ApiError QuestionnaireContent)
    | QuestionnaireMsg Questionnaire.Msg
    | Close


update : Msg -> QuestionnaireDetail -> AppState -> Model -> ( Model, Cmd Msg )
update msg questionnaire appState model =
    case msg of
        FetchPreviewComplete result ->
            case ( result, model.questionnaireModel ) of
                ( Ok content, Loading ) ->
                    let
                        questionnaireModel =
                            QuestionnaireDetail.updateContent questionnaire content
                                |> Questionnaire.init appState
                                |> Success
                    in
                    ( { model | questionnaireModel = questionnaireModel }, Cmd.none )

                ( Err error, Loading ) ->
                    ( { model | questionnaireModel = ApiError.toActionResult appState "Unable to fetch questionnaire." error }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        QuestionnaireMsg questionnaireMsg ->
            let
                updateQuestionnaire =
                    Triple.second
                        << Questionnaire.update
                            questionnaireMsg
                            QuestionnaireMsg
                            Nothing
                            appState
                            { events = [] }
            in
            ( { model | questionnaireModel = ActionResult.map updateQuestionnaire model.questionnaireModel }
            , Cmd.none
            )

        Close ->
            ( { model | questionnaireModel = Unset, eventUuid = Nothing }, Cmd.none )



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
                |> Maybe.andThen (QuestionnaireDetail.getVersionByEventUuid { versions = cfg.versions })
                |> Maybe.unwrap emptyNode QuestionnaireVersionTag.version
    in
    div [ class "QuestionnaireVersionViewModal modal-cover", classList [ ( "visible", visible ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content", dataCy "modal_project-version" ]
                [ div [ class "modal-header" ]
                    [ strong [ class "modal-title" ] [ text datetime, versionBadge ]
                    , button [ class "close", onClick Close ]
                        [ faSet "_global.close" appState ]
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
            }
        , renderer = DefaultQuestionnaireRenderer.create appState qm.questionnaire.knowledgeModel
        , wrapMsg = QuestionnaireMsg
        , previewQuestionnaireEventMsg = Nothing
        , revertQuestionnaireMsg = Nothing
        }
        { events = [] }
        qm
