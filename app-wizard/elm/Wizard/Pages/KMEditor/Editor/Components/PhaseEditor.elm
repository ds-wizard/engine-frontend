module Wizard.Pages.KMEditor.Editor.Components.PhaseEditor exposing (EventMsg, Model, Msg, initialModel, update, view)

import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faKmAnswer, faKmChapter, faKmItemTemplate, faKmQuestion)
import Dict
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, div, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, classList, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import String.Extra as String
import Wizard.Api.Models.Event exposing (Event(..))
import Wizard.Api.Models.Event.CommonEventData exposing (CommonEventData)
import Wizard.Api.Models.Event.EditEventSetters exposing (setRequiredPhaseUuid)
import Wizard.Api.Models.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Wizard.Api.Models.Event.EditQuestionFileEventData as EditQuestionFileEventData
import Wizard.Api.Models.Event.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData
import Wizard.Api.Models.Event.EditQuestionItemSelectData as EditQuestionItemSelectEventData
import Wizard.Api.Models.Event.EditQuestionListEventData as EditQuestionListEventData
import Wizard.Api.Models.Event.EditQuestionMultiChoiceEventData as EditQuestionMultiChoiceEventData
import Wizard.Api.Models.Event.EditQuestionOptionsEventData as EditQuestionOptionsEventData
import Wizard.Api.Models.Event.EditQuestionValueEventData as EditQuestionValueEventData
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)


type alias Model =
    { highlightedPhaseUuid : Maybe String }


initialModel : Model
initialModel =
    { highlightedPhaseUuid = Nothing }


type Msg
    = Highlight String
    | CancelHighlight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Highlight phaseUuid ->
            ( { model | highlightedPhaseUuid = Just phaseUuid }, Cmd.none )

        CancelHighlight ->
            ( { model | highlightedPhaseUuid = Nothing }, Cmd.none )


type alias EventMsg msg =
    String -> Maybe String -> (CommonEventData -> Event) -> msg


type alias SetPhaseEventMsg msg =
    Question -> String -> Maybe String -> msg


view : AppState -> (Msg -> msg) -> EventMsg msg -> EditorBranch -> Model -> Html msg
view appState wrapMsg eventMsg editorBranch model =
    let
        setPhaseEventMsg question parentUuid mbPhaseUuid =
            eventMsg parentUuid (Just (Question.getUuid question)) <|
                EditQuestionEvent <|
                    case question of
                        OptionsQuestion _ _ ->
                            EditQuestionOptionsEventData.init
                                |> setRequiredPhaseUuid mbPhaseUuid
                                |> EditQuestionOptionsEvent

                        ListQuestion _ _ ->
                            EditQuestionListEventData.init
                                |> setRequiredPhaseUuid mbPhaseUuid
                                |> EditQuestionListEvent

                        ValueQuestion _ _ ->
                            EditQuestionValueEventData.init
                                |> setRequiredPhaseUuid mbPhaseUuid
                                |> EditQuestionValueEvent

                        IntegrationQuestion _ _ ->
                            EditQuestionIntegrationEventData.init
                                |> setRequiredPhaseUuid mbPhaseUuid
                                |> EditQuestionIntegrationEvent

                        MultiChoiceQuestion _ _ ->
                            EditQuestionMultiChoiceEventData.init
                                |> setRequiredPhaseUuid mbPhaseUuid
                                |> EditQuestionMultiChoiceEvent

                        ItemSelectQuestion _ _ ->
                            EditQuestionItemSelectEventData.init
                                |> setRequiredPhaseUuid mbPhaseUuid
                                |> EditQuestionItemSelectEvent

                        FileQuestion _ _ ->
                            EditQuestionFileEventData.init
                                |> setRequiredPhaseUuid mbPhaseUuid
                                |> EditQuestionFileEvent

        content =
            if List.isEmpty editorBranch.branch.knowledgeModel.phaseUuids then
                Flash.info (gettext "There are no phases, create them first." appState.locale)

            else if Dict.isEmpty editorBranch.branch.knowledgeModel.entities.questions then
                Flash.info (gettext "There are no questions, create them first." appState.locale)

            else
                let
                    props =
                        { wrapMsg = wrapMsg
                        , setPhaseEventMsg = setPhaseEventMsg
                        , editorBranch = editorBranch
                        }
                in
                phaseEditorTable appState props model
    in
    div [ class "KMEditor__Editor__TableEditor", dataCy "km-editor_phases" ]
        [ content ]


type alias Props msg =
    { wrapMsg : Msg -> msg
    , setPhaseEventMsg : SetPhaseEventMsg msg
    , editorBranch : EditorBranch
    }


phaseEditorTable : AppState -> Props msg -> Model -> Html msg
phaseEditorTable appState props model =
    let
        phases =
            EditorBranch.filterDeletedWith .uuid props.editorBranch <|
                KnowledgeModel.getPhases props.editorBranch.branch.knowledgeModel
    in
    div [ class "editor-table-container" ]
        [ table []
            [ thead []
                [ tr []
                    (th [ class "top-left" ] [ div [] [] ]
                        :: List.map (thPhase appState model) phases
                    )
                ]
            , tbody [] (foldKMRows appState props model phases)
            ]
        ]


thPhase : AppState -> Model -> Phase -> Html msg
thPhase appState model phase =
    let
        attributes =
            [ class "phase"
            , classList [ ( "untitled", untitled ) ]
            , dataCy "km-editor_phase-editor_phase"
            ]

        ( untitled, phaseName ) =
            if String.isEmpty phase.title then
                ( True, gettext "Untitled phase" appState.locale )

            else
                ( False, phase.title )
    in
    th [ class "th-item", classList [ ( "highlighted", model.highlightedPhaseUuid == Just phase.uuid ) ] ]
        [ div []
            [ div attributes [ text phaseName ]
            ]
        ]


foldKMRows : AppState -> Props msg -> Model -> List Phase -> List (Html msg)
foldKMRows appState props model phases =
    let
        chapters =
            EditorBranch.filterDeletedWith .uuid props.editorBranch <|
                KnowledgeModel.getChapters props.editorBranch.branch.knowledgeModel
    in
    List.foldl (\c rows -> rows ++ foldChapter appState props model phases c) [] chapters


foldChapter : AppState -> Props msg -> Model -> List Phase -> Chapter -> List (Html msg)
foldChapter appState props model phases chapter =
    if List.length chapter.questionUuids > 0 then
        let
            questions =
                EditorBranch.filterDeletedWith Question.getUuid props.editorBranch <|
                    KnowledgeModel.getChapterQuestions chapter.uuid props.editorBranch.branch.knowledgeModel
        in
        List.foldl (\q rows -> rows ++ foldQuestion appState props model 1 phases q) [ trChapter appState props chapter phases ] questions

    else
        []


foldQuestion : AppState -> Props msg -> Model -> Int -> List Phase -> Question -> List (Html msg)
foldQuestion appState props model indent phase question =
    let
        questionRow =
            [ trQuestion appState props model indent phase question ]
    in
    case question of
        OptionsQuestion commonData _ ->
            List.foldl
                (\a rows -> rows ++ foldAnswer appState props model (indent + 1) phase a)
                questionRow
                (EditorBranch.filterDeletedWith .uuid props.editorBranch <|
                    KnowledgeModel.getQuestionAnswers commonData.uuid props.editorBranch.branch.knowledgeModel
                )

        ListQuestion commonData _ ->
            List.foldl
                (\q rows -> rows ++ foldQuestion appState props model (indent + 2) phase q)
                (questionRow ++ [ trItemTemplate appState props (indent + 1) phase ])
                (EditorBranch.filterDeletedWith Question.getUuid props.editorBranch <|
                    KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid props.editorBranch.branch.knowledgeModel
                )

        ValueQuestion _ _ ->
            questionRow

        IntegrationQuestion _ _ ->
            questionRow

        MultiChoiceQuestion _ _ ->
            questionRow

        ItemSelectQuestion _ _ ->
            questionRow

        FileQuestion _ _ ->
            questionRow


foldAnswer : AppState -> Props msg -> Model -> Int -> List Phase -> Answer -> List (Html msg)
foldAnswer appState props model indent phases answer =
    let
        followUps =
            EditorBranch.filterDeletedWith Question.getUuid props.editorBranch <|
                KnowledgeModel.getAnswerFollowupQuestions answer.uuid props.editorBranch.branch.knowledgeModel
    in
    if List.length followUps > 0 then
        List.foldl (\q rows -> rows ++ foldQuestion appState props model (indent + 1) phases q) [ trAnswer appState props answer indent phases ] followUps

    else
        []


trQuestion : AppState -> Props msg -> Model -> Int -> List Phase -> Question -> Html msg
trQuestion appState props model indent phases question =
    let
        questionTitle =
            String.withDefault (gettext "Untitled question" appState.locale) (Question.getTitle question)
    in
    tr []
        (th []
            [ div [ indentClass indent ]
                [ linkTo (EditorBranch.editorRoute props.editorBranch (Question.getUuid question))
                    []
                    [ faKmQuestion
                    , text questionTitle
                    ]
                ]
            ]
            :: List.map (tdQuestionTagCheckbox props model question) phases
        )


tdQuestionTagCheckbox : Props msg -> Model -> Question -> Phase -> Html msg
tdQuestionTagCheckbox props model question phase =
    let
        hasPhase =
            Question.getRequiredPhaseUuid question == Just phase.uuid

        newPhase =
            if hasPhase then
                Nothing

            else
                Just phase.uuid

        parentUuid =
            EditorBranch.getParentUuid (Question.getUuid question) props.editorBranch

        msg =
            props.setPhaseEventMsg question parentUuid newPhase
    in
    td
        [ class "td-checkbox"
        , classList [ ( "highlighted", model.highlightedPhaseUuid == Just phase.uuid ) ]
        , onMouseOver <| props.wrapMsg <| Highlight phase.uuid
        , onMouseOut <| props.wrapMsg <| CancelHighlight
        ]
        [ label []
            [ input
                [ type_ "checkbox"
                , checked hasPhase
                , onClick msg
                , dataCy ("km-editor_phase-editor_row_question-" ++ Question.getUuid question ++ "_" ++ "phase-" ++ phase.uuid)
                ]
                []
            ]
        ]


trChapter : AppState -> Props msg -> Chapter -> List Phase -> Html msg
trChapter appState props chapter =
    trSeparator props
        { title = String.withDefault (gettext "Untitled chapter" appState.locale) chapter.title
        , icon = faKmChapter
        , mbExtraClass = Just "separator-chapter"
        , mbEditorUuid = Just chapter.uuid
        }
        0


trAnswer : AppState -> Props msg -> Answer -> Int -> List Phase -> Html msg
trAnswer appState props answer =
    trSeparator props
        { title = String.withDefault (gettext "Untitled answer" appState.locale) answer.label
        , icon = faKmAnswer
        , mbExtraClass = Nothing
        , mbEditorUuid = Just answer.uuid
        }


trItemTemplate : AppState -> Props msg -> Int -> List Phase -> Html msg
trItemTemplate appState props =
    trSeparator props
        { title = gettext "Item Template" appState.locale
        , icon = faKmItemTemplate
        , mbExtraClass = Nothing
        , mbEditorUuid = Nothing
        }


type alias SeparatorProps msg =
    { title : String
    , icon : Html msg
    , mbExtraClass : Maybe String
    , mbEditorUuid : Maybe String
    }


trSeparator : Props msg -> SeparatorProps msg -> Int -> List Phase -> Html msg
trSeparator props { title, icon, mbExtraClass, mbEditorUuid } indent phases =
    let
        createLink content =
            case mbEditorUuid of
                Just editorUuid ->
                    [ linkTo (EditorBranch.editorRoute props.editorBranch editorUuid)
                        []
                        content
                    ]

                Nothing ->
                    content
    in
    tr [ class <| "separator " ++ Maybe.withDefault "" mbExtraClass ]
        (th []
            [ div [ indentClass indent ]
                (createLink
                    [ icon
                    , text title
                    ]
                )
            ]
            :: List.map (tdPhase props) phases
        )


tdPhase : Props msg -> Phase -> Html msg
tdPhase props phase =
    td
        [ onMouseOver <| props.wrapMsg <| Highlight phase.uuid
        , onMouseOut <| props.wrapMsg <| CancelHighlight
        ]
        []


indentClass : Int -> Attribute msg
indentClass indent =
    class <| (++) "indent-" <| String.fromInt indent
