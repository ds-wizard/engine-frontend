module Wizard.Pages.KMEditor.Editor.Components.Preview exposing
    ( Model
    , Msg
    , ViewConfig
    , generateReplies
    , initialModel
    , setActiveChapterIfNot
    , setKnowledgeModel
    , setKnowledgeModelPackageId
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
import Wizard.Api.Models.ProjectDetail.Reply exposing (Reply)
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire as Questionnaire exposing (ActivePage(..))
import Wizard.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Components.Tag as Tag
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)
import Wizard.Routes


type alias Model =
    { questionnaireModel : Questionnaire.Model
    , tags : Set String
    }


initialModel : AppState -> String -> Model
initialModel appState kmPackageId =
    let
        questionnaire =
            createQuestionnaireDetail kmPackageId KnowledgeModel.empty
    in
    { questionnaireModel = initQuestionnaireModel appState questionnaire
    , tags = Set.empty
    }


initQuestionnaireModel : AppState -> ProjectQuestionnaire -> Questionnaire.Model
initQuestionnaireModel appState questionnaire =
    let
        ( questionnaireModel, _ ) =
            Questionnaire.initSimple appState questionnaire

        viewSettings =
            questionnaireModel.viewSettings
    in
    { questionnaireModel | viewSettings = { viewSettings | answeredBy = False } }


setActiveChapterIfNot : String -> Model -> Model
setActiveChapterIfNot uuid model =
    if not (String.isEmpty uuid) && model.questionnaireModel.activePage == PageNone then
        { model | questionnaireModel = Questionnaire.setActiveChapterUuid uuid model.questionnaireModel }

    else
        model


setPhase : Maybe Uuid -> Model -> Model
setPhase mbPhaseUuid model =
    if Maybe.isNothing model.questionnaireModel.questionnaire.phaseUuid then
        { model | questionnaireModel = Questionnaire.setPhaseUuid mbPhaseUuid model.questionnaireModel }

    else
        model


setKnowledgeModelPackageId : AppState -> String -> Model -> Model
setKnowledgeModelPackageId appState kmPackageId model =
    let
        questionnaire =
            createQuestionnaireDetail kmPackageId KnowledgeModel.empty

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
        questionnaireModel =
            model.questionnaireModel

        ( newSeed, mbChapterUuid, questionnaireDetail ) =
            ProjectQuestionnaire.generateReplies appState.currentTime appState.seed questionUuid knowledgeModel questionnaireModel.questionnaire

        activePage =
            Maybe.unwrap questionnaireModel.activePage PageChapter mbChapterUuid
    in
    ( newSeed
    , { model
        | questionnaireModel =
            { questionnaireModel
                | activePage = activePage
                , questionnaire = questionnaireDetail
            }
      }
    )


createQuestionnaireDetail : String -> KnowledgeModel -> ProjectQuestionnaire
createQuestionnaireDetail kmPackageId km =
    let
        kmPackage =
            KnowledgeModelPackage.dummy
    in
    ProjectQuestionnaire.createQuestionnaireDetail { kmPackage | id = kmPackageId } km


questionnaireModelWithKnowledgeModel : KnowledgeModel -> Questionnaire.Model -> Questionnaire.Model
questionnaireModelWithKnowledgeModel km questionnaireModel =
    let
        questionnaire =
            questionnaireModel.questionnaire
    in
    { questionnaireModel
        | questionnaire = { questionnaire | knowledgeModel = km }
        , knowledgeModelParentMap = KnowledgeModel.createParentMap km
    }


type Msg
    = QuestionnaireMsg Questionnaire.Msg
    | AddTag String
    | RemoveTag String
    | SelectAllTags
    | SelectNoneTags


update : Msg -> AppState -> EditorContext -> Model -> ( Seed, Model, Cmd Msg )
update msg appState editorContext model =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg appState editorContext model

        AddTag uuid ->
            ( appState.seed, { model | tags = Set.insert uuid model.tags }, Cmd.none )

        RemoveTag uuid ->
            ( appState.seed, { model | tags = Set.remove uuid model.tags }, Cmd.none )

        SelectAllTags ->
            ( appState.seed, { model | tags = Set.fromList <| EditorContext.filterDeleted editorContext editorContext.kmEditor.knowledgeModel.tagUuids }, Cmd.none )

        SelectNoneTags ->
            ( appState.seed, { model | tags = Set.empty }, Cmd.none )


handleQuestionnaireMsg : Questionnaire.Msg -> AppState -> EditorContext -> Model -> ( Seed, Model, Cmd Msg )
handleQuestionnaireMsg msg appState editorContext model =
    let
        ( newSeed, newQuestionnaireModel, cmd ) =
            Questionnaire.update msg
                QuestionnaireMsg
                Nothing
                appState
                { events = editorContext.kmEditor.events
                , kmEditorUuid = Just editorContext.kmEditor.uuid
                }
                model.questionnaireModel
    in
    ( newSeed
    , { model | questionnaireModel = newQuestionnaireModel }
    , cmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map QuestionnaireMsg <|
        Questionnaire.subscriptions model.questionnaireModel


type alias ViewConfig msg =
    { editorContext : EditorContext
    , wrapMsg : Msg -> msg
    , saveRepliesMsg : msg
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState { editorContext, wrapMsg, saveRepliesMsg } model =
    let
        knowledgeModel =
            model.questionnaireModel.questionnaire.knowledgeModel

        questionnaireModel =
            model.questionnaireModel

        questionnaire =
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = False
                    , todosEnabled = False
                    , commentsEnabled = False
                    , pluginsEnabled = False
                    , readonly = False
                    , toolbarEnabled = False
                    , questionLinksEnabled = False
                    }
                , renderer =
                    DefaultQuestionnaireRenderer.create appState
                        (DefaultQuestionnaireRenderer.config model.questionnaireModel.questionnaire
                            |> DefaultQuestionnaireRenderer.withResourcePageToRoute (Wizard.Routes.kmEditorEditor editorContext.kmEditor.uuid << Just << Uuid.fromUuidString)
                        )
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                , isKmEditor = True
                , projectCommon = Nothing
                }
                { events = []
                , kmEditorUuid = Just editorContext.kmEditor.uuid
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
    List.length model.questionnaireModel.questionnaire.knowledgeModel.tagUuids > 0


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
