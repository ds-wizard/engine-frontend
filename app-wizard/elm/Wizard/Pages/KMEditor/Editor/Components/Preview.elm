module Wizard.Pages.KMEditor.Editor.Components.Preview exposing
    ( Model
    , Msg
    , ViewConfig
    , generateReplies
    , initialModel
    , scrollToQuestion
    , setActiveChapterIfNot
    , setKnowledgeModel
    , setKnowledgeModelPackageUuid
    , setPhase
    , setReplies
    , subscriptions
    , update
    , view
    )

import Common.Components.FontAwesome exposing (fas)
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Set exposing (Set)
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Wizard.Api.Models.ProjectCommon as ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.ProjectDetail.Reply exposing (Reply)
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire2 as Questionnaire2
import Wizard.Components.Tag as Tag
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)


type alias Model =
    { kmEditorUuid : Uuid
    , projectCommon : ProjectCommon
    , questionnaireModel : Questionnaire2.Model
    , tags : Set String
    }


initialModel : AppState -> Uuid -> Uuid -> Model
initialModel appState kmEditorUuid kmPackageUuid =
    let
        questionnaire =
            createQuestionnaireDetail kmPackageUuid KnowledgeModel.empty
    in
    { kmEditorUuid = kmEditorUuid
    , projectCommon = ProjectCommon.dummy
    , questionnaireModel = initQuestionnaireModel appState questionnaire
    , tags = Set.empty
    }


initQuestionnaireModel : AppState -> ProjectQuestionnaire -> Questionnaire2.Model
initQuestionnaireModel appState questionnaire =
    let
        ( questionnaireModel, _ ) =
            Questionnaire2.initSimple appState questionnaire

        viewSettings =
            questionnaireModel.viewSettings
    in
    { questionnaireModel | viewSettings = { viewSettings | answeredBy = False } }


setActiveChapterIfNot : AppState -> String -> Model -> Model
setActiveChapterIfNot appState chapterUuid model =
    if model.questionnaireModel.chapterUuid == Uuid.toString Uuid.nil then
        let
            questionnaireReturnData =
                Questionnaire2.update appState
                    { wrapMsg = QuestionnaireMsg
                    , mbKmEditorUuid = Just model.kmEditorUuid
                    , mbSetFullScreenMsg = Nothing
                    , projectCommon = model.projectCommon
                    }
                    (Questionnaire2.openChapterMsg chapterUuid)
                    model.questionnaireModel
        in
        { model | questionnaireModel = questionnaireReturnData.model }

    else
        model


setPhase : AppState -> Maybe Uuid -> Model -> Model
setPhase appState mbPhaseUuid model =
    if Maybe.isNothing model.questionnaireModel.questionnaire.phaseUuid then
        let
            questionnaireReturnData =
                Questionnaire2.update appState
                    { wrapMsg = QuestionnaireMsg
                    , mbKmEditorUuid = Just model.kmEditorUuid
                    , mbSetFullScreenMsg = Nothing
                    , projectCommon = model.projectCommon
                    }
                    (Questionnaire2.setPhaseMsg mbPhaseUuid)
                    model.questionnaireModel
        in
        { model | questionnaireModel = questionnaireReturnData.model }

    else
        model


setKnowledgeModelPackageUuid : AppState -> Uuid -> Model -> Model
setKnowledgeModelPackageUuid appState kmPackageUuid model =
    let
        questionnaire =
            createQuestionnaireDetail kmPackageUuid KnowledgeModel.empty

        questionnaireModel =
            initQuestionnaireModel appState questionnaire
    in
    { model | questionnaireModel = questionnaireModel }


setKnowledgeModel : KnowledgeModel -> Model -> Model
setKnowledgeModel km model =
    { model
        | questionnaireModel =
            questionnaireModelWithKnowledgeModel km model.questionnaireModel
    }


setReplies : Dict String Reply -> Model -> Model
setReplies replies model =
    let
        questionnaire =
            model.questionnaireModel.questionnaire

        questionnaireModel =
            model.questionnaireModel
    in
    { model | questionnaireModel = { questionnaireModel | questionnaire = { questionnaire | replies = replies } } }


generateReplies : AppState -> String -> KnowledgeModel -> Model -> ( Seed, Model )
generateReplies appState questionUuid knowledgeModel model =
    let
        ( newSeed, mbChapterUuid, questionnaireDetail ) =
            ProjectQuestionnaire.generateReplies appState.currentTime appState.seed questionUuid knowledgeModel model.questionnaireModel.questionnaire

        questionnaireModel =
            model.questionnaireModel

        questionnaireReturnData =
            Questionnaire2.update appState
                { wrapMsg = QuestionnaireMsg
                , mbKmEditorUuid = Just model.kmEditorUuid
                , mbSetFullScreenMsg = Nothing
                , projectCommon = model.projectCommon
                }
                (Questionnaire2.openChapterMsg (Maybe.withDefault (Uuid.toString Uuid.nil) mbChapterUuid))
                { questionnaireModel | questionnaire = questionnaireDetail }
    in
    ( newSeed
    , { model | questionnaireModel = questionnaireReturnData.model }
    )


createQuestionnaireDetail : Uuid -> KnowledgeModel -> ProjectQuestionnaire
createQuestionnaireDetail kmPackageUuid km =
    let
        kmPackage =
            KnowledgeModelPackage.dummy
    in
    ProjectQuestionnaire.createQuestionnaireDetail { kmPackage | uuid = kmPackageUuid } km


questionnaireModelWithKnowledgeModel : KnowledgeModel -> Questionnaire2.Model -> Questionnaire2.Model
questionnaireModelWithKnowledgeModel km questionnaireModel =
    let
        questionnaire =
            questionnaireModel.questionnaire
    in
    Questionnaire2.virtualizeContent
        { questionnaireModel
            | questionnaire = { questionnaire | knowledgeModel = km }
            , knowledgeModelParentMap = KnowledgeModel.createParentMap km
        }


type Msg
    = QuestionnaireMsg Questionnaire2.Msg
    | AddTag String
    | RemoveTag String
    | SelectAllTags
    | SelectNoneTags


update : Msg -> AppState -> EditorContext -> Model -> ( Seed, Model, Cmd Msg )
update msg appState editorContext model =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg appState model

        AddTag uuid ->
            ( appState.seed, { model | tags = Set.insert uuid model.tags }, Cmd.none )

        RemoveTag uuid ->
            ( appState.seed, { model | tags = Set.remove uuid model.tags }, Cmd.none )

        SelectAllTags ->
            ( appState.seed, { model | tags = Set.fromList <| EditorContext.filterDeleted editorContext editorContext.kmEditor.knowledgeModel.tagUuids }, Cmd.none )

        SelectNoneTags ->
            ( appState.seed, { model | tags = Set.empty }, Cmd.none )


scrollToQuestion : String -> Cmd Msg
scrollToQuestion questionUuid =
    Questionnaire2.dispatchScrollToQuestion QuestionnaireMsg questionUuid


handleQuestionnaireMsg : Questionnaire2.Msg -> AppState -> Model -> ( Seed, Model, Cmd Msg )
handleQuestionnaireMsg msg appState model =
    let
        questionnaireReturnData =
            Questionnaire2.update appState
                { wrapMsg = QuestionnaireMsg
                , mbKmEditorUuid = Just model.kmEditorUuid
                , mbSetFullScreenMsg = Nothing
                , projectCommon = model.projectCommon
                }
                msg
                model.questionnaireModel
    in
    ( questionnaireReturnData.seed
    , { model | questionnaireModel = questionnaireReturnData.model }
    , questionnaireReturnData.cmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map QuestionnaireMsg <|
        Questionnaire2.subscriptions model.questionnaireModel


type alias ViewConfig msg =
    { editorContext : EditorContext
    , wrapMsg : Msg -> msg
    , saveRepliesMsg : msg
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState { wrapMsg, saveRepliesMsg } model =
    let
        knowledgeModel =
            model.questionnaireModel.questionnaire.knowledgeModel

        questionnaireModel =
            model.questionnaireModel

        questionnaire =
            Questionnaire2.view appState
                { wrapMsg = QuestionnaireMsg
                , readonly = False
                , toolbarEnabled = False
                , actionsEnabled = False
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                }
                (questionnaireModelWithKnowledgeModel (KnowledgeModel.filterWithTags (Set.toList model.tags) knowledgeModel) questionnaireModel)
    in
    div
        [ class "col KMEditor__Editor__Preview"
        , classList [ ( "KMEditor__Editor__Preview--WithTags", tagSelectionVisible model ) ]
        , dataCy "km-editor_preview"
        ]
        [ toolbar appState wrapMsg saveRepliesMsg model
        , Html.map wrapMsg questionnaire
        ]


toolbar : AppState -> (Msg -> msg) -> msg -> Model -> Html msg
toolbar appState wrapMsg saveRepliesMsg model =
    let
        tagHeader =
            if tagSelectionVisible model then
                Html.map wrapMsg <|
                    div [ class "d-flex align-items-center mb-1" ]
                        [ strong [ class "me-1" ] [ text (gettext "Tags" appState.locale) ]
                        , a
                            [ onClick SelectAllTags
                            , dataCy "km-editor_preview_tags_select-all"
                            , class "btn btn-link"
                            ]
                            [ fas "fa-check-double me-1"
                            , text (gettext "Select all" appState.locale)
                            ]
                        , a
                            [ onClick SelectNoneTags
                            , dataCy "km-editor_preview_tags_select-none"
                            , class "btn btn-link"
                            ]
                            [ fas "fa-times me-1"
                            , text (gettext "Select none" appState.locale)
                            ]
                        ]

            else
                div [] []
    in
    div [ class "toolbar" ]
        [ div [ class "d-flex justify-content-between" ]
            [ tagHeader
            , div []
                [ a
                    [ class "btn btn-link ms-3"
                    , onClick saveRepliesMsg
                    , dataCy "km-editor_preview_save-values"
                    ]
                    [ fas "fa-save me-1"
                    , text (gettext "Save preview values" appState.locale)
                    ]
                ]
            ]
        , Html.map wrapMsg <| tagSelection appState model
        ]


tagSelectionVisible : Model -> Bool
tagSelectionVisible model =
    not (List.isEmpty model.questionnaireModel.questionnaire.knowledgeModel.tagUuids)


tagSelection : AppState -> Model -> Html Msg
tagSelection appState model =
    if tagSelectionVisible model then
        let
            tags =
                KnowledgeModel.getTags model.questionnaireModel.questionnaire.knowledgeModel

            tagListConfig =
                { selected = Set.toList model.tags
                , addMsg = AddTag
                , removeMsg = RemoveTag
                , showDescription = False
                }
        in
        div [ class "tag-selection" ]
            [ Tag.list appState tagListConfig tags
            ]

    else
        Html.nothing
