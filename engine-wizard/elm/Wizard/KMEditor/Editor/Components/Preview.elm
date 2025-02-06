module Wizard.KMEditor.Editor.Components.Preview exposing
    ( Model
    , Msg
    , ViewConfig
    , generateReplies
    , initialModel
    , setActiveChapterIfNot
    , setPackageId
    , setPhase
    , setReplies
    , subscriptions
    , update
    , view
    )

import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Registry.Components.FontAwesome exposing (fas)
import Set exposing (Set)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.Package as Package
import Shared.Data.QuestionnaireDetail.Reply exposing (Reply)
import Shared.Data.QuestionnaireQuestionnaire as QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Shared.Html exposing (emptyNode)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire exposing (ActivePage(..))
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Tag as Tag
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.Routes


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
    { questionnaireModel = initQuestionnaireModel appState questionnaire
    , tags = Set.empty
    }


initQuestionnaireModel : AppState -> QuestionnaireQuestionnaire -> Questionnaire.Model
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


setPackageId : AppState -> String -> Model -> Model
setPackageId appState packageId model =
    let
        questionnaire =
            createQuestionnaireDetail packageId KnowledgeModel.empty

        questionnaireModel =
            initQuestionnaireModel appState questionnaire
    in
    { model | questionnaireModel = questionnaireModel }


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
            QuestionnaireQuestionnaire.generateReplies appState.currentTime appState.seed questionUuid knowledgeModel questionnaireModel.questionnaire

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


createQuestionnaireDetail : String -> KnowledgeModel -> QuestionnaireQuestionnaire
createQuestionnaireDetail packageId km =
    let
        package =
            Package.dummy
    in
    QuestionnaireQuestionnaire.createQuestionnaireDetail { package | id = packageId } km


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


type alias ViewConfig msg =
    { editorBranch : EditorBranch
    , wrapMsg : Msg -> msg
    , saveRepliesMsg : msg
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState { editorBranch, wrapMsg, saveRepliesMsg } model =
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
                    , questionLinksEnabled = False
                    }
                , renderer =
                    DefaultQuestionnaireRenderer.create appState
                        knowledgeModel
                        (Wizard.Routes.kmEditorEditor editorBranch.branch.uuid << Just << Uuid.fromUuidString)
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                , isKmEditor = True
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
        [ toolbar appState wrapMsg saveRepliesMsg model knowledgeModel
        , Html.map wrapMsg questionnaire
        ]


toolbar : AppState -> (Msg -> msg) -> msg -> Model -> KnowledgeModel -> Html msg
toolbar appState wrapMsg saveRepliesMsg model km =
    let
        tagHeader =
            if tagSelectionVisible km then
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
        , Html.map wrapMsg <| tagSelection appState model.tags km
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
                , showDescription = False
                }
        in
        div [ class "tag-selection" ]
            [ Tag.list appState tagListConfig tags
            ]

    else
        emptyNode
