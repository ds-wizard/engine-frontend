module Wizard.Pages.KMEditor.Editor.Components.TagEditor exposing
    ( EventMsg
    , Model
    , Msg
    , initialModel
    , update
    , view
    )

import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faKmAnswer, faKmChapter, faKmItemTemplate, faKmQuestion)
import Common.Utils.ColorUtils as ColorUtils
import Dict
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, div, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, classList, style, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import String.Extra as String
import Wizard.Api.Models.Event exposing (Event(..))
import Wizard.Api.Models.Event.CommonEventData exposing (CommonEventData)
import Wizard.Api.Models.Event.EditEventSetters exposing (setTagUuids)
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
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)


type alias Model =
    { highlightedTagUuid : Maybe String }


initialModel : Model
initialModel =
    { highlightedTagUuid = Nothing }


type Msg
    = Highlight String
    | CancelHighlight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Highlight tagUuid ->
            ( { model | highlightedTagUuid = Just tagUuid }, Cmd.none )

        CancelHighlight ->
            ( { model | highlightedTagUuid = Nothing }, Cmd.none )


type alias EventMsg msg =
    String -> Maybe String -> (CommonEventData -> Event) -> msg


type alias SetTagsEventMsg msg =
    Question -> String -> List String -> msg


view : AppState -> (Msg -> msg) -> EventMsg msg -> EditorContext -> Model -> Html msg
view appState wrapMsg eventMsg editorContext model =
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

                        ItemSelectQuestion _ _ ->
                            EditQuestionItemSelectEventData.init
                                |> setTagUuids tagUuids
                                |> EditQuestionItemSelectEvent

                        FileQuestion _ _ ->
                            EditQuestionFileEventData.init
                                |> setTagUuids tagUuids
                                |> EditQuestionFileEvent

        content =
            if List.isEmpty editorContext.kmEditor.knowledgeModel.tagUuids then
                Flash.info (gettext "There are no question tags, create them first." appState.locale)

            else if Dict.isEmpty editorContext.kmEditor.knowledgeModel.entities.questions then
                Flash.info (gettext "There are no questions, create them first." appState.locale)

            else
                let
                    props =
                        { wrapMsg = wrapMsg
                        , setTagsEventMsg = setTagsEventMsg
                        , editorContext = editorContext
                        }
                in
                tagEditorTable appState props model
    in
    div [ class "KMEditor__Editor__TableEditor", dataCy "km-editor_tags" ]
        [ content ]


type alias Props msg =
    { wrapMsg : Msg -> msg
    , setTagsEventMsg : SetTagsEventMsg msg
    , editorContext : EditorContext
    }


tagEditorTable : AppState -> Props msg -> Model -> Html msg
tagEditorTable appState props model =
    let
        tags =
            EditorContext.filterDeletedWith .uuid props.editorContext <|
                KnowledgeModel.getTags props.editorContext.kmEditor.knowledgeModel
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
            , style "color" <| ColorUtils.getContrastColorHex tag.color
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
    th [ class "th-item", classList [ ( "highlighted", model.highlightedTagUuid == Just tag.uuid ) ] ]
        [ div []
            [ div attributes [ text tagName ]
            ]
        ]


foldKMRows : AppState -> Props msg -> Model -> List Tag -> List (Html msg)
foldKMRows appState props model tags =
    let
        chapters =
            EditorContext.filterDeletedWith .uuid props.editorContext <|
                KnowledgeModel.getChapters props.editorContext.kmEditor.knowledgeModel
    in
    List.foldl (\c rows -> rows ++ foldChapter appState props model tags c) [] chapters


foldChapter : AppState -> Props msg -> Model -> List Tag -> Chapter -> List (Html msg)
foldChapter appState props model tags chapter =
    if List.length chapter.questionUuids > 0 then
        let
            questions =
                EditorContext.filterDeletedWith Question.getUuid props.editorContext <|
                    KnowledgeModel.getChapterQuestions chapter.uuid props.editorContext.kmEditor.knowledgeModel
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
                (EditorContext.filterDeletedWith .uuid props.editorContext <|
                    KnowledgeModel.getQuestionAnswers commonData.uuid props.editorContext.kmEditor.knowledgeModel
                )

        ListQuestion commonData _ ->
            List.foldl
                (\q rows -> rows ++ foldQuestion appState props model (indent + 2) tags q)
                (questionRow ++ [ trItemTemplate appState props (indent + 1) tags ])
                (EditorContext.filterDeletedWith Question.getUuid props.editorContext <|
                    KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid props.editorContext.kmEditor.knowledgeModel
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


foldAnswer : AppState -> Props msg -> Model -> Int -> List Tag -> Answer -> List (Html msg)
foldAnswer appState props model indent tags answer =
    let
        followUps =
            EditorContext.filterDeletedWith Question.getUuid props.editorContext <|
                KnowledgeModel.getAnswerFollowupQuestions answer.uuid props.editorContext.kmEditor.knowledgeModel
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
        (th []
            [ div [ indentClass indent ]
                [ linkTo (EditorContext.editorRoute props.editorContext (Question.getUuid question))
                    []
                    [ faKmQuestion
                    , text questionTitle
                    ]
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
            EditorContext.getParentUuid (Question.getUuid question) props.editorContext

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
        { title = String.withDefault (gettext "Untitled chapter" appState.locale) chapter.title
        , icon = faKmChapter
        , mbExtraClass = Just "separator-chapter"
        , mbEditorUuid = Just chapter.uuid
        }
        0


trAnswer : AppState -> Props msg -> Answer -> Int -> List Tag -> Html msg
trAnswer appState props answer =
    trSeparator props
        { title = String.withDefault (gettext "Untitled answer" appState.locale) answer.label
        , icon = faKmAnswer
        , mbExtraClass = Nothing
        , mbEditorUuid = Just answer.uuid
        }


trItemTemplate : AppState -> Props msg -> Int -> List Tag -> Html msg
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


trSeparator : Props msg -> SeparatorProps msg -> Int -> List Tag -> Html msg
trSeparator props { title, icon, mbExtraClass, mbEditorUuid } indent tags =
    let
        createLink content =
            case mbEditorUuid of
                Just editorUuid ->
                    [ linkTo (EditorContext.editorRoute props.editorContext editorUuid)
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
