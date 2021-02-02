module Wizard.Common.Components.Questionnaire exposing
    ( Context
    , Model
    , Msg(..)
    , QuestionnaireRenderer
    , clearReply
    , init
    , setActiveChapterUuid
    , setLabels
    , setLevel
    , setReply
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Debounce exposing (Debounce)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseDown)
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Roman
import Shared.Api.TypeHints as TypeHintsApi
import Shared.Data.Event exposing (Event)
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Shared.Data.QuestionnaireDetail.ReplyValue.IntegrationReplyValue exposing (IntegrationReplyValue(..))
import Shared.Data.TypeHint exposing (TypeHint)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode, faKeyClass, faSet)
import Shared.Locale exposing (l, lf, lg, lgx, lx)
import Shared.Utils exposing (dispatch, flip, getUuid, listFilterJust, listInsertIf)
import String exposing (fromInt)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire.FeedbackModal as FeedbackModal
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Common.View.Page as Page
import Wizard.Common.View.Tag as Tag
import Wizard.Ports as Ports


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Questionnaire"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Common.Components.Questionnaire"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.Questionnaire"


type alias Model =
    { uuid : Uuid
    , activePage : ActivePage
    , questionnaire : QuestionnaireDetail
    , typeHints : Maybe TypeHints
    , typeHintsDebounce : Debounce ( List String, String, String )
    , feedbackModalModel : FeedbackModal.Model
    }


type alias TypeHints =
    { path : List String
    , hints : ActionResult (List TypeHint)
    }


type ActivePage
    = PageNone
    | PageChapter String


init : QuestionnaireDetail -> Model
init questionnaire =
    { uuid = questionnaire.uuid
    , activePage = PageNone
    , questionnaire = questionnaire
    , typeHints = Nothing
    , typeHintsDebounce = Debounce.init
    , feedbackModalModel = FeedbackModal.init
    }


setActiveChapterUuid : String -> Model -> Model
setActiveChapterUuid uuid model =
    { model | activePage = PageChapter uuid }


setLevel : Int -> Model -> Model
setLevel level =
    updateQuestionnaire <| QuestionnaireDetail.setLevel level


setReply : String -> ReplyValue -> Model -> Model
setReply path replyValue =
    updateQuestionnaire <| QuestionnaireDetail.setReplyValue path replyValue


clearReply : String -> Model -> Model
clearReply path =
    updateQuestionnaire <| QuestionnaireDetail.clearReplyValue path


setLabels : String -> List String -> Model -> Model
setLabels path value =
    updateQuestionnaire <| QuestionnaireDetail.setLabels path value


updateQuestionnaire : (QuestionnaireDetail -> QuestionnaireDetail) -> Model -> Model
updateQuestionnaire fn model =
    { model | questionnaire = fn model.questionnaire }


type alias Config =
    { features : FeaturesConfig
    , renderer : QuestionnaireRenderer Msg
    }


type alias FeaturesConfig =
    { feedbackEnabled : Bool
    , todosEnabled : Bool
    , readonly : Bool
    }


type alias QuestionnaireRenderer msg =
    { renderQuestionLabel : Question -> Html msg
    , renderQuestionDescription : Question -> Html msg
    , getQuestionExtraClass : Question -> Maybe String
    , renderAnswerLabel : Answer -> Html msg
    , renderAnswerBadges : Answer -> Html msg
    , renderAnswerAdvice : Answer -> Html msg
    , renderChoiceLabel : Choice -> Html msg
    }


type alias Context =
    { levels : List Level
    , metrics : List Metric
    , events : List Event
    }


type QuestionViewState
    = Default
    | Answered
    | Desirable



-- UPDATE


type Msg
    = SetActivePage ActivePage
    | ScrollToTodo QuestionnaireTodo
    | ShowTypeHints (List String) String String
    | HideTypeHints
    | TypeHintInput (List String) ReplyValue
    | TypeHintDebounceMsg Debounce.Msg
    | TypeHintsLoaded (List String) (Result ApiError (List TypeHint))
    | FeedbackModalMsg FeedbackModal.Msg
    | SetLevel String
    | SetReply String ReplyValue
    | ClearReply String
    | AddItem String (List String)
    | SetLabels String (List String)


update : Msg -> AppState -> Context -> Model -> ( Seed, Model, Cmd Msg )
update msg appState ctx model =
    let
        withSeed ( newModel, cmd ) =
            ( appState.seed, newModel, cmd )

        wrap newModel =
            ( appState.seed, newModel, Cmd.none )
    in
    case msg of
        SetActivePage activePage ->
            withSeed <| ( { model | activePage = activePage }, Cmd.none )

        ScrollToTodo todo ->
            withSeed <| handleScrollToTodo model todo

        ShowTypeHints path questionUuid value ->
            withSeed <| handleShowTypeHints appState ctx model path questionUuid value

        HideTypeHints ->
            wrap { model | typeHints = Nothing }

        TypeHintInput path value ->
            withSeed <| handleTypeHintsInput model path value

        TypeHintDebounceMsg debounceMsg ->
            withSeed <| handleTypeHintDebounceMsg appState ctx model debounceMsg

        TypeHintsLoaded path result ->
            wrap <| handleTypeHintsLoaded appState model path result

        FeedbackModalMsg feedbackModalMsg ->
            withSeed <| handleFeedbackModalMsg appState model feedbackModalMsg

        SetLevel levelString ->
            wrap <| setLevel (Maybe.withDefault 1 (String.toInt levelString)) model

        SetReply path replyValue ->
            wrap <| setReply path replyValue model

        ClearReply path ->
            wrap <| clearReply path model

        AddItem path originalItems ->
            handleAddItem appState model path originalItems

        SetLabels path value ->
            wrap <| setLabels path value model


handleScrollToTodo : Model -> QuestionnaireTodo -> ( Model, Cmd Msg )
handleScrollToTodo model todo =
    let
        selector =
            "[data-path=\"" ++ todo.path ++ "\"]"
    in
    ( { model | activePage = PageChapter todo.chapter.uuid }, Ports.scrollIntoView selector )


handleShowTypeHints : AppState -> Context -> Model -> List String -> String -> String -> ( Model, Cmd Msg )
handleShowTypeHints appState ctx model path questionUuid value =
    let
        typeHints =
            Just
                { path = path
                , hints = Loading
                }

        cmd =
            loadTypeHints appState ctx model path questionUuid value
    in
    ( { model | typeHints = typeHints }, cmd )


handleTypeHintsInput : Model -> List String -> ReplyValue -> ( Model, Cmd Msg )
handleTypeHintsInput model path value =
    let
        questionUuid =
            Maybe.withDefault "" (List.last path)

        ( debounce, debounceCmd ) =
            Debounce.push
                debounceConfig
                ( path, questionUuid, ReplyValue.getStringReply value )
                model.typeHintsDebounce

        dispatchCmd =
            dispatch <|
                SetReply (String.join "." path) value
    in
    ( { model | typeHintsDebounce = debounce }
    , Cmd.batch [ debounceCmd, dispatchCmd ]
    )


handleTypeHintDebounceMsg : AppState -> Context -> Model -> Debounce.Msg -> ( Model, Cmd Msg )
handleTypeHintDebounceMsg appState ctx model debounceMsg =
    let
        load ( path, questionUuid, value ) =
            loadTypeHints appState ctx model path questionUuid value

        ( typeHintsDebounce, cmd ) =
            Debounce.update
                debounceConfig
                (Debounce.takeLast load)
                debounceMsg
                model.typeHintsDebounce
    in
    ( { model | typeHintsDebounce = typeHintsDebounce }, cmd )


handleTypeHintsLoaded : AppState -> Model -> List String -> Result ApiError (List TypeHint) -> Model
handleTypeHintsLoaded appState model path result =
    case model.typeHints of
        Just typeHints ->
            if typeHints.path == path then
                case result of
                    Ok hints ->
                        { model | typeHints = Just { typeHints | hints = Success hints } }

                    Err _ ->
                        { model | typeHints = Just { typeHints | hints = Error <| lg "apiError.typeHints.getListError" appState } }

            else
                model

        _ ->
            model


handleFeedbackModalMsg : AppState -> Model -> FeedbackModal.Msg -> ( Model, Cmd Msg )
handleFeedbackModalMsg appState model feedbackModalMsg =
    let
        ( feedbackModalModel, cmd ) =
            FeedbackModal.update feedbackModalMsg appState model.feedbackModalModel
    in
    ( { model | feedbackModalModel = feedbackModalModel }
    , Cmd.map FeedbackModalMsg cmd
    )


handleAddItem : AppState -> Model -> String -> List String -> ( Seed, Model, Cmd Msg )
handleAddItem appState model path originalItems =
    let
        ( uuid, newSeed ) =
            getUuid appState.seed

        dispatchCmd =
            dispatch <|
                SetReply path <|
                    ItemListReply (originalItems ++ [ uuid ])
    in
    ( newSeed, model, dispatchCmd )


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = TypeHintDebounceMsg
    }


loadTypeHints : AppState -> Context -> Model -> List String -> String -> String -> Cmd Msg
loadTypeHints appState ctx model path questionUuid value =
    TypeHintsApi.fetchTypeHints
        (Just model.questionnaire.package.id)
        ctx.events
        questionUuid
        value
        appState
        (TypeHintsLoaded path)



-- VIEW


view : AppState -> Config -> Context -> Model -> Html Msg
view appState cfg ctx model =
    div [ class "questionnaire" ]
        [ viewQuestionnairePanel appState cfg ctx model
        , viewQuestionnaireContent appState cfg ctx model
        , Html.map FeedbackModalMsg <| FeedbackModal.view appState model.feedbackModalModel
        ]



-- QUESTIONNAIRE - PANEL


viewQuestionnairePanel : AppState -> Config -> Context -> Model -> Html Msg
viewQuestionnairePanel appState cfg ctx model =
    div [ class "questionnaire__panel" ]
        [ viewQuestionnairePanelPhaseSelection appState cfg ctx model
        , viewQuestionnairePanelChapters appState model
        ]



-- QUESTIONNAIRE - PANEL - PHASE SELECTION


viewQuestionnairePanelPhaseSelection : AppState -> Config -> Context -> Model -> Html Msg
viewQuestionnairePanelPhaseSelection appState cfg ctx model =
    if appState.config.questionnaire.levels.enabled then
        let
            selectAttrs =
                if cfg.features.readonly then
                    [ disabled True ]

                else
                    [ onInput SetLevel ]
        in
        div [ class "questionnaire__panel__phase" ]
            [ label [] [ lgx "questionnaire.currentPhase" appState ]
            , select (class "form-control" :: selectAttrs)
                (List.map (viewQuestionnairePanelPhaseSelectionOption model.questionnaire.level) ctx.levels)
            ]

    else
        emptyNode


viewQuestionnairePanelPhaseSelectionOption : Int -> Level -> Html Msg
viewQuestionnairePanelPhaseSelectionOption selectedLevel level =
    option [ value (fromInt level.level), selected (selectedLevel == level.level) ]
        [ text level.title ]



-- QUESTIONNAIRE - PANEL - CHAPTERS


viewQuestionnairePanelChapters : AppState -> Model -> Html Msg
viewQuestionnairePanelChapters appState model =
    let
        mbActiveChapterUuid =
            case model.activePage of
                PageChapter chapterUuid ->
                    Just chapterUuid

                _ ->
                    Nothing

        chapters =
            KnowledgeModel.getChapters model.questionnaire.knowledgeModel
    in
    div [ class "questionnaire__panel__chapters" ]
        [ strong [] [ lgx "chapters" appState ]
        , div [ class "nav nav-pills flex-column" ]
            (List.indexedMap (viewQuestionnairePanelChaptersChapter appState model mbActiveChapterUuid) chapters)
        ]


viewQuestionnairePanelChaptersChapter : AppState -> Model -> Maybe String -> Int -> Chapter -> Html Msg
viewQuestionnairePanelChaptersChapter appState model mbActiveChapterUuid order chapter =
    a
        [ class "nav-link"
        , classList [ ( "active", mbActiveChapterUuid == Just chapter.uuid ) ]
        , onClick (SetActivePage (PageChapter chapter.uuid))
        ]
        [ span [ class "chapter-number" ] [ text (Roman.toRomanNumber (order + 1) ++ ". ") ]
        , span [ class "chapter-name" ] [ text chapter.title ]
        , viewQuestionnairePanelChaptersChapterIndication appState model.questionnaire chapter
        ]


viewQuestionnairePanelChaptersChapterIndication : AppState -> QuestionnaireDetail -> Chapter -> Html Msg
viewQuestionnairePanelChaptersChapterIndication appState questionnaire chapter =
    let
        effectiveLevel =
            if appState.config.questionnaire.levels.enabled then
                questionnaire.level

            else
                100

        unanswered =
            QuestionnaireDetail.calculateUnansweredQuestionsForChapter
                questionnaire
                effectiveLevel
                chapter
    in
    if unanswered > 0 then
        span [ class "badge badge-light badge-pill" ] [ text <| fromInt unanswered ]

    else
        faSet "questionnaire.answeredIndication" appState



-- QUESTIONNAIRE -- CONTENT


viewQuestionnaireContent : AppState -> Config -> Context -> Model -> Html Msg
viewQuestionnaireContent appState cfg ctx model =
    let
        content =
            case model.activePage of
                PageChapter chapterUuid ->
                    case KnowledgeModel.getChapter chapterUuid model.questionnaire.knowledgeModel of
                        Just chapter ->
                            viewQuestionnaireContentChapter appState cfg ctx model chapter

                        Nothing ->
                            emptyNode

                _ ->
                    emptyNode
    in
    div [ class "questionnaire__content" ] [ content ]



-- QUESTIONNAIRE -- CONTENT -- CHAPTER


viewQuestionnaireContentChapter : AppState -> Config -> Context -> Model -> Chapter -> Html Msg
viewQuestionnaireContentChapter appState cfg ctx model chapter =
    let
        chapterNumber =
            KnowledgeModel.getChapters model.questionnaire.knowledgeModel
                |> List.findIndex (.uuid >> (==) chapter.uuid)
                |> Maybe.unwrap "I" ((+) 1 >> Roman.toRomanNumber)

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid model.questionnaire.knowledgeModel

        questionViews =
            List.indexedMap (viewQuestion appState cfg ctx model [ chapter.uuid ] []) questions
    in
    div [ class "questionnaire__form container" ]
        [ h2 [] [ text (chapterNumber ++ ". " ++ chapter.title) ]
        , Markdown.toHtml [ class "chapter-description" ] (Maybe.withDefault "" chapter.text)
        , div [] questionViews
        ]


viewQuestion : AppState -> Config -> Context -> Model -> List String -> List String -> Int -> Question -> Html Msg
viewQuestion appState cfg ctx model path humanIdentifiers order question =
    let
        newHumanIdentifiers =
            humanIdentifiers ++ [ String.fromInt (order + 1) ]

        newPath =
            path ++ [ Question.getUuid question ]

        ( viewInput, viewExtensions ) =
            case question of
                OptionsQuestion _ _ ->
                    viewQuestionOptions appState cfg ctx model newPath newHumanIdentifiers question

                ListQuestion _ _ ->
                    ( viewQuestionList appState cfg ctx model newPath newHumanIdentifiers question, [] )

                ValueQuestion _ _ ->
                    ( viewQuestionValue cfg model newPath question, [] )

                IntegrationQuestion _ _ ->
                    ( viewQuestionIntegration appState cfg model newPath question, [] )

                MultiChoiceQuestion _ _ ->
                    ( viewQuestionMultiChoice cfg model newPath question, [] )

        viewLabel =
            viewQuestionLabel appState cfg ctx model newPath newHumanIdentifiers question

        viewTags =
            let
                tags =
                    Question.getTagUuids question
                        |> List.map (flip KnowledgeModel.getTag model.questionnaire.knowledgeModel)
                        |> listFilterJust
                        |> List.sortBy .name
            in
            Tag.viewList tags

        viewDescription =
            cfg.renderer.renderQuestionDescription question

        content =
            if Question.isList question || Question.isOptions question || Question.isMultiChoice question then
                viewLabel :: viewTags :: viewDescription :: viewInput :: viewExtensions

            else
                viewLabel :: viewTags :: viewInput :: viewDescription :: viewExtensions

        questionExtraClass =
            Maybe.withDefault "" (cfg.renderer.getQuestionExtraClass question)
    in
    div
        [ class ("form-group " ++ questionExtraClass)
        , id ("question-" ++ Question.getUuid question)
        , attribute "data-path" (pathToString newPath)
        ]
        content


viewQuestionLabel : AppState -> Config -> Context -> Model -> List String -> List String -> Question -> Html Msg
viewQuestionLabel appState cfg ctx model path humanIdentifiers question =
    let
        questionState =
            case
                ( QuestionnaireDetail.hasReply (pathToString path) model.questionnaire
                , Question.isDesirable appState model.questionnaire.level question
                )
            of
                ( True, _ ) ->
                    Answered

                ( _, True ) ->
                    Desirable

                _ ->
                    Default
    in
    label []
        [ span []
            [ span
                [ class "badge badge-secondary badge-human-identifier"
                , classList
                    [ ( "badge-secondary", questionState == Default )
                    , ( "badge-success", questionState == Answered )
                    , ( "badge-danger", questionState == Desirable )
                    ]
                ]
                [ text (String.join "." humanIdentifiers) ]
            , span
                [ classList
                    [ ( "text-success", questionState == Answered )
                    , ( "text-danger", questionState == Desirable )
                    ]
                ]
                [ cfg.renderer.renderQuestionLabel question ]
            ]
        , span [ class "custom-actions" ]
            [ viewTodoAction appState cfg model path
            , viewFeedbackAction appState cfg model question
            ]
        ]


viewQuestionOptions : AppState -> Config -> Context -> Model -> List String -> List String -> Question -> ( Html Msg, List (Html Msg) )
viewQuestionOptions appState cfg ctx model path humanIdentifiers question =
    let
        answers =
            KnowledgeModel.getQuestionAnswers (Question.getUuid question) model.questionnaire.knowledgeModel

        selectedAnswerUuid =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.map ReplyValue.getAnswerUuid

        mbSelectedAnswer =
            List.find (.uuid >> Just >> (==) selectedAnswerUuid) answers

        clearReplyButton =
            viewQuestionOptionsClearButton appState cfg path mbSelectedAnswer

        advice =
            Maybe.unwrap emptyNode cfg.renderer.renderAnswerAdvice mbSelectedAnswer

        followUps =
            Maybe.unwrap emptyNode
                (viewQuestionOptionsFollowUps appState cfg ctx model answers path humanIdentifiers)
                mbSelectedAnswer
    in
    ( div []
        (List.indexedMap (viewAnswer appState cfg path selectedAnswerUuid) answers
            ++ [ clearReplyButton ]
        )
    , [ advice, followUps ]
    )


viewQuestionOptionsClearButton : AppState -> Config -> List String -> Maybe Answer -> Html Msg
viewQuestionOptionsClearButton appState cfg path mbSelectedAnswer =
    if cfg.features.readonly || Maybe.isNothing mbSelectedAnswer then
        emptyNode

    else
        a [ class "clear-answer", onClick (ClearReply (pathToString path)) ]
            [ faSet "questionnaire.clearAnswer" appState
            , lx_ "answer.clear" appState
            ]


viewQuestionOptionsFollowUps : AppState -> Config -> Context -> Model -> List Answer -> List String -> List String -> Answer -> Html Msg
viewQuestionOptionsFollowUps appState cfg ctx model answers path humanIdentifiers answer =
    let
        index =
            Maybe.unwrap "a" identifierToChar <|
                List.findIndex (.uuid >> (==) answer.uuid) answers

        newPath =
            path ++ [ answer.uuid ]

        newHumanIdentifier =
            humanIdentifiers ++ [ index ]

        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid model.questionnaire.knowledgeModel

        followUpQuestions =
            List.indexedMap (viewQuestion appState cfg ctx model newPath newHumanIdentifier) questions
    in
    if List.isEmpty followUpQuestions then
        emptyNode

    else
        div [ class "followups-group" ] followUpQuestions


viewQuestionMultiChoice : Config -> Model -> List String -> Question -> Html Msg
viewQuestionMultiChoice cfg model path question =
    let
        choices =
            KnowledgeModel.getQuestionChoices (Question.getUuid question) model.questionnaire.knowledgeModel

        selectedChoicesUuids =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap [] ReplyValue.getChoiceUuid
    in
    div [] (List.indexedMap (viewChoice cfg path selectedChoicesUuids) choices)


viewQuestionList : AppState -> Config -> Context -> Model -> List String -> List String -> Question -> Html Msg
viewQuestionList appState cfg ctx model path humanIdentifiers question =
    let
        viewItem =
            viewQuestionListItem appState cfg ctx model question itemUuids path humanIdentifiers

        itemUuids =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap [] ReplyValue.getItemUuids

        noAnswersInfo =
            if cfg.features.readonly && List.isEmpty itemUuids then
                i [] [ lx_ "list.noAnswers" appState ]

            else
                emptyNode
    in
    div []
        [ div [] (List.indexedMap viewItem itemUuids)
        , viewQuestionListAdd appState cfg itemUuids path
        , noAnswersInfo
        ]


viewQuestionListAdd : AppState -> Config -> List String -> List String -> Html Msg
viewQuestionListAdd appState cfg itemUuids path =
    if cfg.features.readonly then
        emptyNode

    else
        button
            [ class "btn btn-outline-secondary link-with-icon"
            , onClick (AddItem (pathToString path) itemUuids)
            ]
            [ faSet "_global.add" appState
            , lx_ "list.add" appState
            ]


viewQuestionListItem : AppState -> Config -> Context -> Model -> Question -> List String -> List String -> List String -> Int -> String -> Html Msg
viewQuestionListItem appState cfg ctx model question itemUuids path humanIdentifiers index uuid =
    let
        newItems =
            List.filter ((/=) uuid) itemUuids

        newPath =
            path ++ [ uuid ]

        newHumanIdentifiers =
            humanIdentifiers ++ [ identifierToChar index ]

        questions =
            KnowledgeModel.getQuestionItemTemplateQuestions (Question.getUuid question) model.questionnaire.knowledgeModel

        itemQuestions =
            List.indexedMap (viewQuestion appState cfg ctx model newPath newHumanIdentifiers) questions

        deleteButton =
            if cfg.features.readonly then
                emptyNode

            else
                button
                    [ class "btn btn-outline-danger btn-item-delete"
                    , onClick (SetReply (pathToString path) (ItemListReply newItems))
                    ]
                    [ faSet "_global.delete" appState ]
    in
    div [ class "item" ]
        [ div [ class "card bg-light mb-5" ]
            [ div [ class "card-body" ] itemQuestions
            ]
        , deleteButton
        ]


viewQuestionValue : Config -> Model -> List String -> Question -> Html Msg
viewQuestionValue cfg model path question =
    let
        answer =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap "" ReplyValue.getStringReply

        defaultAttrs =
            [ class "form-control", value answer ]

        extraAttrs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onInput (SetReply (pathToString path) << StringReply) ]

        inputView =
            case Question.getValueType question of
                Just NumberQuestionValueType ->
                    input (type_ "number" :: defaultAttrs ++ extraAttrs) []

                Just TextQuestionValueType ->
                    textarea (defaultAttrs ++ extraAttrs) []

                _ ->
                    input (type_ "text" :: defaultAttrs ++ extraAttrs) []
    in
    div [] [ inputView ]


viewQuestionIntegration : AppState -> Config -> Model -> List String -> Question -> Html Msg
viewQuestionIntegration appState cfg model path question =
    let
        extraArgs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onInput (TypeHintInput path << IntegrationReply << PlainValue)
                , onFocus (ShowTypeHints path (Question.getUuid question) questionValue)
                , onBlur HideTypeHints
                ]

        mbReply =
            Dict.get (pathToString path) model.questionnaire.replies

        questionValue =
            Maybe.unwrap "" ReplyValue.getStringReply mbReply

        integrationUuid =
            Maybe.withDefault "" <| Question.getIntegrationUuid question

        integration =
            KnowledgeModel.getIntegration integrationUuid model.questionnaire.knowledgeModel

        typeHintsVisible =
            Maybe.unwrap False (.path >> (==) path) model.typeHints

        viewTypeHints =
            if typeHintsVisible then
                viewQuestionIntegrationTypeHints appState cfg model path

            else
                emptyNode
    in
    div []
        [ input ([ class "form-control", type_ "text", value questionValue ] ++ extraArgs) []
        , viewTypeHints
        , viewQuestionIntegrationReplyExtra integration mbReply
        ]


viewQuestionIntegrationTypeHints : AppState -> Config -> Model -> List String -> Html Msg
viewQuestionIntegrationTypeHints appState cfg model path =
    let
        content =
            case Maybe.unwrap Unset .hints model.typeHints of
                Success hints ->
                    ul [] (List.map (viewQuestionIntegrationTypeHint cfg path) hints)

                Loading ->
                    div [ class "loading" ]
                        [ faSet "_global.spinner" appState
                        , lx_ "typeHints.loading" appState
                        ]

                Error err ->
                    div [ class "error" ]
                        [ faSet "_global.error" appState
                        , text err
                        ]

                Unset ->
                    emptyNode
    in
    div [ class "typehints" ] [ content ]


viewQuestionIntegrationTypeHint : Config -> List String -> TypeHint -> Html Msg
viewQuestionIntegrationTypeHint cfg path typeHint =
    if cfg.features.readonly then
        emptyNode

    else
        li []
            [ a
                [ onMouseDown <| SetReply (pathToString path) <| IntegrationReply <| IntegrationValue typeHint.id typeHint.name ]
                [ text typeHint.name
                ]
            ]


viewQuestionIntegrationReplyExtra : Maybe Integration -> Maybe ReplyValue -> Html Msg
viewQuestionIntegrationReplyExtra mbIntegration mbReplyValue =
    case ( mbIntegration, mbReplyValue ) of
        ( Just integration, Just (IntegrationReply (IntegrationValue id _)) ) ->
            let
                url =
                    String.replace "${id}" id integration.itemUrl

                logo =
                    if String.isEmpty integration.logo then
                        emptyNode

                    else
                        img [ src integration.logo ] []
            in
            p [ class "integration-extra" ]
                [ logo
                , a [ href url, target "_blank" ] [ text url ]
                ]

        _ ->
            emptyNode


viewChoice : Config -> List String -> List String -> Int -> Choice -> Html Msg
viewChoice cfg path selectedChoicesUuids order choice =
    let
        checkboxName =
            pathToString (path ++ [ choice.uuid ])

        humanIdentifier =
            identifierToChar order ++ ". "

        isSelected =
            List.member choice.uuid selectedChoicesUuids

        extraArgs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                let
                    newSelectedUuids =
                        if isSelected then
                            List.filter ((/=) choice.uuid) selectedChoicesUuids

                        else
                            choice.uuid :: selectedChoicesUuids
                in
                [ onClick (SetReply (pathToString path) (MultiChoiceReply newSelectedUuids)) ]
    in
    div
        [ class "radio"
        , classList [ ( "radio-selected", isSelected ), ( "radio-disabled", cfg.features.readonly ) ]
        ]
        [ label []
            [ input ([ type_ "checkbox", name checkboxName, checked isSelected ] ++ extraArgs) []
            , text humanIdentifier
            , cfg.renderer.renderChoiceLabel choice
            ]
        ]


viewAnswer : AppState -> Config -> List String -> Maybe String -> Int -> Answer -> Html Msg
viewAnswer appState cfg path selectedAnswerUuid order answer =
    let
        radioName =
            pathToString (path ++ [ answer.uuid ])

        humanIdentifier =
            identifierToChar order ++ ". "

        extraArgs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onClick (SetReply (pathToString path) (AnswerReply answer.uuid)) ]

        followUpsIndicator =
            if List.isEmpty answer.followUpUuids then
                emptyNode

            else
                i
                    [ class ("expand-icon " ++ faKeyClass "questionnaire.followUpsIndication" appState)
                    , title (l_ "answer.followUpTitle" appState)
                    ]
                    []

        isSelected =
            selectedAnswerUuid == Just answer.uuid
    in
    div
        [ class "radio"
        , classList [ ( "radio-selected", isSelected ), ( "radio-disabled", cfg.features.readonly ) ]
        ]
        [ label []
            [ input ([ type_ "radio", name radioName, checked isSelected ] ++ extraArgs) []
            , text humanIdentifier
            , cfg.renderer.renderAnswerLabel answer
            , followUpsIndicator
            , cfg.renderer.renderAnswerBadges answer
            ]
        ]


viewTodoAction : AppState -> Config -> Model -> List String -> Html Msg
viewTodoAction appState cfg model path =
    let
        currentPath =
            pathToString path

        hasTodo =
            model.questionnaire.labels
                |> Dict.get currentPath
                |> Maybe.unwrap False (List.member QuestionnaireDetail.todoUuid)

        todoButton =
            span [ class "action action-todo" ]
                [ span [] [ lx_ "todoAction.todo" appState ]
                , a
                    [ title <| l_ "todoAction.remove" appState
                    , onClick <| SetLabels currentPath []
                    ]
                    [ faSet "_global.remove" appState ]
                ]

        addTodoButton =
            a
                [ class "action action-add-todo"
                , onClick <| SetLabels currentPath [ QuestionnaireDetail.todoUuid ]
                ]
                [ faSet "_global.add" appState
                , span [] [ span [] [ lx_ "todoAction.add" appState ] ]
                ]
    in
    if cfg.features.todosEnabled then
        if hasTodo then
            todoButton

        else
            addTodoButton

    else
        emptyNode


viewFeedbackAction : AppState -> Config -> Model -> Question -> Html Msg
viewFeedbackAction appState cfg model question =
    let
        openFeedbackModal =
            FeedbackModalMsg (FeedbackModal.OpenFeedback model.questionnaire.package.id (Question.getUuid question))

        feedbackEnabled =
            appState.config.questionnaire.feedback.enabled && cfg.features.feedbackEnabled
    in
    if feedbackEnabled then
        a
            [ class "action"
            , attribute "data-cy" "feedback"
            , onClick openFeedbackModal
            ]
            [ faSet "questionnaire.feedback" appState ]

    else
        emptyNode



-- UTILS


pathToString : List String -> String
pathToString =
    String.join "."


identifierToChar : Int -> String
identifierToChar =
    (+) 97 >> Char.fromCode >> String.fromChar
