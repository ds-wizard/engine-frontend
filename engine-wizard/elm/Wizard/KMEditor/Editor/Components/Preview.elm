module Wizard.KMEditor.Editor.Components.Preview exposing
    ( Model
    , Msg
    , initialModel
    , setActiveChapterIfNot
    , setPhase
    , subscriptions
    , update
    , view
    )

import Html exposing (Html, a, div, strong)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Set exposing (Set)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.Package as Package
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (lgx, lx)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire exposing (ActivePage(..))
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Tag as Tag
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.Components.Preview"


type alias Model =
    { questionnaireModel : Questionnaire.Model
    , tags : Set String
    }


initialModel : AppState -> String -> Model
initialModel appState packageId =
    let
        questionnaire =
            createQuestionnaireDetail packageId KnowledgeModel.empty
    in
    { questionnaireModel = Questionnaire.init appState questionnaire
    , tags = Set.empty
    }


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


createQuestionnaireDetail : String -> KnowledgeModel -> QuestionnaireDetail
createQuestionnaireDetail packageId km =
    let
        package =
            Package.dummy
    in
    QuestionnaireDetail.createQuestionnaireDetail { package | id = packageId } km


type Msg
    = QuestionnaireMsg Questionnaire.Msg
    | AddTag String
    | RemoveTag String
    | SelectAllTags
    | SelectNoneTags


update : Msg -> AppState -> EditorBranch -> Model -> ( Seed, Model, Cmd Msg )
update msg appState editorBranch model =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg appState editorBranch model

        AddTag uuid ->
            ( appState.seed, { model | tags = Set.insert uuid model.tags }, Cmd.none )

        RemoveTag uuid ->
            ( appState.seed, { model | tags = Set.remove uuid model.tags }, Cmd.none )

        SelectAllTags ->
            ( appState.seed, { model | tags = Set.fromList <| EditorBranch.filterDeleted editorBranch editorBranch.branch.knowledgeModel.tagUuids }, Cmd.none )

        SelectNoneTags ->
            ( appState.seed, { model | tags = Set.empty }, Cmd.none )


handleQuestionnaireMsg : Questionnaire.Msg -> AppState -> EditorBranch -> Model -> ( Seed, Model, Cmd Msg )
handleQuestionnaireMsg msg appState editorBranch model =
    let
        ( newSeed, newQuestionnaireModel, cmd ) =
            Questionnaire.update msg
                QuestionnaireMsg
                Nothing
                appState
                { events = editorBranch.branch.events }
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


view : AppState -> EditorBranch -> Model -> Html Msg
view appState editorBranch model =
    let
        knowledgeModel =
            EditorBranch.getFilteredKM editorBranch

        questionnaireDetail =
            model.questionnaireModel.questionnaire

        questionnaireModel =
            model.questionnaireModel

        questionnaire =
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = False
                    , todosEnabled = False
                    , commentsEnabled = False
                    , readonly = False
                    , toolbarEnabled = False
                    }
                , renderer = DefaultQuestionnaireRenderer.create appState knowledgeModel
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                }
                { events = []
                }
                { questionnaireModel
                    | questionnaire =
                        { questionnaireDetail
                            | knowledgeModel = KnowledgeModel.filterWithTags (Set.toList model.tags) knowledgeModel
                        }
                }
    in
    div
        [ class "col KMEditor__Editor__Preview"
        , classList [ ( "KMEditor__Editor__Preview--WithTags", tagSelectionVisible knowledgeModel ) ]
        , dataCy "km-editor_preview"
        ]
        [ tagSelection appState model.tags knowledgeModel
        , questionnaire
        ]


tagSelectionVisible : KnowledgeModel -> Bool
tagSelectionVisible km =
    List.length km.tagUuids > 0


tagSelection : AppState -> Set String -> KnowledgeModel -> Html Msg
tagSelection appState selected knowledgeModel =
    if tagSelectionVisible knowledgeModel then
        let
            tags =
                KnowledgeModel.getTags knowledgeModel

            tagListConfig =
                { selected = Set.toList selected
                , addMsg = AddTag
                , removeMsg = RemoveTag
                }
        in
        div [ class "tag-selection" ]
            [ div [ class "tag-selection-header" ]
                [ strong [] [ lgx "tags" appState ]
                , a [ onClick SelectAllTags, dataCy "km-editor_preview_tags_select-all" ] [ lx_ "tags.selectAll" appState ]
                , a [ onClick SelectNoneTags, dataCy "km-editor_preview_tags_select-none" ] [ lx_ "tags.selectNone" appState ]
                ]
            , Tag.list appState tagListConfig tags
            ]

    else
        emptyNode
