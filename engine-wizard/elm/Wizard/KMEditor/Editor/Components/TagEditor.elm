module Wizard.KMEditor.Editor.Components.TagEditor exposing
    ( EventMsg
    , Model
    , Msg
    , initialModel
    , update
    , view
    )

import Dict
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, div, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, classList, style, type_)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Shared.Copy as Copy
import Shared.Data.Event exposing (Event(..))
import Shared.Data.Event.CommonEventData exposing (CommonEventData)
import Shared.Data.Event.EditEventSetters exposing (setTagUuids)
import Shared.Data.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Shared.Data.Event.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData
import Shared.Data.Event.EditQuestionListEventData as EditQuestionListEventData
import Shared.Data.Event.EditQuestionMultiChoiceEventData as EditQuestionMultiChoiceEventData
import Shared.Data.Event.EditQuestionOptionsEventData as EditQuestionOptionsEventData
import Shared.Data.Event.EditQuestionValueEventData as EditQuestionValueEventData
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Html exposing (faSet)
import Shared.Utils exposing (getContrastColorHex)
import String.Extra as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Flash as Flash
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)


type alias Model =
    { highlightedTagUuid : Maybe String }


initialModel : Model
initialModel =
    { highlightedTagUuid = Nothing }


type Msg
    = Highlight String
    | CancelHighlight
    | CopyUuid String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Highlight tagUuid ->
            ( { model | highlightedTagUuid = Just tagUuid }, Cmd.none )

        CancelHighlight ->
            ( { model | highlightedTagUuid = Nothing }, Cmd.none )

        CopyUuid uuid ->
            ( model, Copy.copyToClipboard uuid )


type alias EventMsg msg =
    String -> Maybe String -> (CommonEventData -> Event) -> msg


type alias SetTagsEventMsg msg =
    Question -> String -> List String -> msg


view : AppState -> (Msg -> msg) -> EventMsg msg -> EditorBranch -> Model -> Html msg
view appState wrapMsg eventMsg editorBranch model =
    let
        setTagsEventMsg question parentUuid tagUuids =
            eventMsg parentUuid (Just (Question.getUuid question)) <|
                EditQuestionEvent <|
                    case question of
                        OptionsQuestion _ _ ->
                            EditQuestionOptionsEventData.init
                                |> setTagUuids tagUuids
                                |> EditQuestionOptionsEvent

                        ListQuestion _ _ ->
                            EditQuestionListEventData.init
                                |> setTagUuids tagUuids
                                |> EditQuestionListEvent

                        ValueQuestion _ _ ->
                            EditQuestionValueEventData.init
                                |> setTagUuids tagUuids
                                |> EditQuestionValueEvent

                        IntegrationQuestion _ _ ->
                            EditQuestionIntegrationEventData.init
                                |> setTagUuids tagUuids
                                |> EditQuestionIntegrationEvent

                        MultiChoiceQuestion _ _ ->
                            EditQuestionMultiChoiceEventData.init
                                |> setTagUuids tagUuids
                                |> EditQuestionMultiChoiceEvent

        content =
            if List.isEmpty editorBranch.branch.knowledgeModel.tagUuids then
                Flash.info appState (gettext "There are no question tags, create them first." appState.locale)

            else if Dict.isEmpty editorBranch.branch.knowledgeModel.entities.questions then
                Flash.info appState (gettext "There are no questions, create them first." appState.locale)

            else
                let
                    props =
                        { wrapMsg = wrapMsg
                        , setTagsEventMsg = setTagsEventMsg
                        , editorBranch = editorBranch
                        }
                in
                tagEditorTable appState props model
    in
    div [ class "KMEditor__Editor__TagEditor", dataCy "km-editor_tags" ]
        [ content ]


type alias Props msg =
    { wrapMsg : Msg -> msg
    , setTagsEventMsg : SetTagsEventMsg msg
    , editorBranch : EditorBranch
    }


tagEditorTable : AppState -> Props msg -> Model -> Html msg
tagEditorTable appState props model =
    let
        tags =
            EditorBranch.filterDeletedWith .uuid props.editorBranch <|
                KnowledgeModel.getTags props.editorBranch.branch.knowledgeModel
    in
    div [ class "editor-table-container" ]
        [ table []
            [ thead []
                [ tr []
                    (th [ class "top-left" ] [ div [] [] ]
                        :: (List.map (thTag appState model) <| List.sortBy .name tags)
                    )
                ]
            , tbody [] (foldKMRows appState props model tags)
            ]
        ]


thTag : AppState -> Model -> Tag -> Html msg
thTag appState model tag =
    let
        attributes =
            [ style "background" tag.color
            , style "color" <| getContrastColorHex tag.color
            , class "tag"
            , classList [ ( "untitled", untitled ) ]
            , dataCy "km-editor_tag-editor_tag"
            ]

        ( untitled, tagName ) =
            if String.isEmpty tag.name then
                ( True, gettext "Untitled tag" appState.locale )

            else
                ( False, tag.name )
    in
    th [ class "th-tag", classList [ ( "highlighted", model.highlightedTagUuid == Just tag.uuid ) ] ]
        [ div []
            [ div attributes [ text tagName ]
            ]
        ]


foldKMRows : AppState -> Props msg -> Model -> List Tag -> List (Html msg)
foldKMRows appState props model tags =
    let
        chapters =
            EditorBranch.filterDeletedWith .uuid props.editorBranch <|
                KnowledgeModel.getChapters props.editorBranch.branch.knowledgeModel
    in
    List.foldl (\c rows -> rows ++ foldChapter appState props model tags c) [] chapters


foldChapter : AppState -> Props msg -> Model -> List Tag -> Chapter -> List (Html msg)
foldChapter appState props model tags chapter =
    if List.length chapter.questionUuids > 0 then
        let
            questions =
                EditorBranch.filterDeletedWith Question.getUuid props.editorBranch <|
                    KnowledgeModel.getChapterQuestions chapter.uuid props.editorBranch.branch.knowledgeModel
        in
        List.foldl (\q rows -> rows ++ foldQuestion appState props model 1 tags q) [ trChapter appState props chapter tags ] questions

    else
        []


foldQuestion : AppState -> Props msg -> Model -> Int -> List Tag -> Question -> List (Html msg)
foldQuestion appState props model indent tags question =
    let
        questionRow =
            [ trQuestion appState props model indent tags question ]
    in
    case question of
        OptionsQuestion commonData _ ->
            List.foldl
                (\a rows -> rows ++ foldAnswer appState props model (indent + 1) tags a)
                questionRow
                (EditorBranch.filterDeletedWith .uuid props.editorBranch <|
                    KnowledgeModel.getQuestionAnswers commonData.uuid props.editorBranch.branch.knowledgeModel
                )

        ListQuestion commonData _ ->
            List.foldl
                (\q rows -> rows ++ foldQuestion appState props model (indent + 2) tags q)
                (questionRow ++ [ trItemTemplate appState props (indent + 1) tags ])
                (EditorBranch.filterDeletedWith Question.getUuid props.editorBranch <|
                    KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid props.editorBranch.branch.knowledgeModel
                )

        ValueQuestion _ _ ->
            questionRow

        IntegrationQuestion _ _ ->
            questionRow

        MultiChoiceQuestion _ _ ->
            questionRow


foldAnswer : AppState -> Props msg -> Model -> Int -> List Tag -> Answer -> List (Html msg)
foldAnswer appState props model indent tags answer =
    let
        followUps =
            EditorBranch.filterDeletedWith Question.getUuid props.editorBranch <|
                KnowledgeModel.getAnswerFollowupQuestions answer.uuid props.editorBranch.branch.knowledgeModel
    in
    if List.length followUps > 0 then
        List.foldl (\q rows -> rows ++ foldQuestion appState props model (indent + 1) tags q) [ trAnswer appState props answer indent tags ] followUps

    else
        []


trQuestion : AppState -> Props msg -> Model -> Int -> List Tag -> Question -> Html msg
trQuestion appState props model indent tags question =
    let
        questionTitle =
            String.withDefault (gettext "Untitled question" appState.locale) (Question.getTitle question)
    in
    tr []
        (th [ onClick <| props.wrapMsg <| CopyUuid <| Question.getUuid question ]
            [ div [ indentClass indent ]
                [ faSet "km.question" appState
                , text questionTitle
                ]
            ]
            :: (List.map (tdQuestionTagCheckbox props model question) <| List.sortBy .name tags)
        )


tdQuestionTagCheckbox : Props msg -> Model -> Question -> Tag -> Html msg
tdQuestionTagCheckbox props model question tag =
    let
        hasTag =
            List.member tag.uuid <|
                Question.getTagUuids question

        newTags =
            if hasTag then
                List.filter ((/=) tag.uuid) (Question.getTagUuids question)

            else
                tag.uuid :: Question.getTagUuids question

        parentUuid =
            EditorBranch.getParentUuid (Question.getUuid question) props.editorBranch

        msg =
            props.setTagsEventMsg question parentUuid newTags
    in
    td
        [ class "td-checkbox"
        , classList [ ( "highlighted", model.highlightedTagUuid == Just tag.uuid ) ]
        , onMouseOver <| props.wrapMsg <| Highlight tag.uuid
        , onMouseOut <| props.wrapMsg <| CancelHighlight
        ]
        [ label []
            [ input
                [ type_ "checkbox"
                , checked hasTag
                , onClick msg
                , dataCy ("km-editor_tag-editor_row_question-" ++ Question.getUuid question ++ "_" ++ "tag-" ++ tag.uuid)
                ]
                []
            ]
        ]


trChapter : AppState -> Props msg -> Chapter -> List Tag -> Html msg
trChapter appState props chapter =
    trSeparator props
        (Just chapter.uuid)
        (String.withDefault (gettext "Untitled chapter" appState.locale) chapter.title)
        (faSet "km.chapter" appState)
        "separator-chapter"
        0


trAnswer : AppState -> Props msg -> Answer -> Int -> List Tag -> Html msg
trAnswer appState props answer =
    trSeparator props
        (Just answer.uuid)
        (String.withDefault (gettext "Untitled answer" appState.locale) answer.label)
        (faSet "km.answer" appState)
        ""


trItemTemplate : AppState -> Props msg -> Int -> List Tag -> Html msg
trItemTemplate appState props =
    trSeparator props
        Nothing
        (gettext "Item Template" appState.locale)
        (faSet "km.itemTemplate" appState)
        ""


trSeparator : Props msg -> Maybe String -> String -> Html msg -> String -> Int -> List Tag -> Html msg
trSeparator props mbUuid title icon extraClass indent tags =
    let
        thAttributes =
            case mbUuid of
                Just uuid ->
                    [ onClick <| props.wrapMsg <| CopyUuid uuid ]

                Nothing ->
                    []
    in
    tr [ class <| "separator " ++ extraClass ]
        (th thAttributes
            [ div [ indentClass indent ]
                [ icon
                , text title
                ]
            ]
            :: (List.map (tdTag props) <| List.sortBy .name tags)
        )


tdTag : Props msg -> Tag -> Html msg
tdTag props tag =
    td
        [ onMouseOver <| props.wrapMsg <| Highlight tag.uuid
        , onMouseOut <| props.wrapMsg <| CancelHighlight
        ]
        []


indentClass : Int -> Attribute msg
indentClass indent =
    class <| (++) "indent-" <| String.fromInt indent
