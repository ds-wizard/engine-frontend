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
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseDown)
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Roman
import Shared.Api.TypeHints as TypeHintsApi
import Shared.Common.TimeUtils as TimeUtils
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
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.Reply exposing (Reply)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType exposing (IntegrationReplyType(..))
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Data.TypeHint exposing (TypeHint)
import Shared.Data.User as User
import Shared.Data.UserInfo as UserInfo
import Shared.Error.ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode, fa, faKeyClass, faSet)
import Shared.Locale exposing (l, lf, lg, lgx, lh, lx)
import Shared.Utils exposing (dispatch, flip, getUuidString, listFilterJust)
import String exposing (fromInt)
import Time.Distance as Time
import Uuid exposing (Uuid)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire.DeleteVersionModal as DeleteVersionModal
import Wizard.Common.Components.Questionnaire.FeedbackModal as FeedbackModal
import Wizard.Common.Components.Questionnaire.History as History
import Wizard.Common.Components.Questionnaire.QuestionnaireViewSettings as QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Common.Components.Questionnaire.VersionModal as VersionModal
import Wizard.Common.Html exposing (illustratedMessage)
import Wizard.Common.TimeDistance as TimeDistance
import Wizard.Common.View.Tag as Tag
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireTodoGroup as QuestionnaireTodoGroup


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Questionnaire"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Common.Components.Questionnaire"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Common.Components.Questionnaire"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.Questionnaire"



-- MODEL


type alias Model =
    { uuid : Uuid
    , activePage : ActivePage
    , rightPanel : RightPanel
    , questionnaire : QuestionnaireDetail
    , typeHints : Maybe TypeHints
    , typeHintsDebounce : Debounce ( List String, String, String )
    , feedbackModalModel : FeedbackModal.Model
    , viewSettings : QuestionnaireViewSettings
    , viewSettingsDropdown : Dropdown.State
    , historyModel : History.Model
    , versionModalModel : VersionModal.Model
    , deleteVersionModalModel : DeleteVersionModal.Model
    }


type alias TypeHints =
    { path : List String
    , hints : ActionResult (List TypeHint)
    }


type ActivePage
    = PageNone
    | PageChapter String


type RightPanel
    = RightPanelNone
    | RightPanelTODOs
    | RightPanelHistory


init : AppState -> QuestionnaireDetail -> Model
init appState questionnaire =
    let
        activePage =
            case List.head (KnowledgeModel.getChapters questionnaire.knowledgeModel) of
                Just chapter ->
                    PageChapter chapter.uuid

                Nothing ->
                    PageNone
    in
    { uuid = questionnaire.uuid
    , activePage = activePage
    , rightPanel = RightPanelNone
    , questionnaire = questionnaire
    , typeHints = Nothing
    , typeHintsDebounce = Debounce.init
    , feedbackModalModel = FeedbackModal.init
    , viewSettings = QuestionnaireViewSettings.all
    , viewSettingsDropdown = Dropdown.initialState
    , historyModel = History.init appState
    , versionModalModel = VersionModal.init
    , deleteVersionModalModel = DeleteVersionModal.init
    }


setActiveChapterUuid : String -> Model -> Model
setActiveChapterUuid uuid model =
    { model | activePage = PageChapter uuid }


setLevel : Int -> Model -> Model
setLevel level =
    updateQuestionnaire <| QuestionnaireDetail.setLevel level


setReply : String -> Reply -> Model -> Model
setReply path reply =
    updateQuestionnaire <| QuestionnaireDetail.setReply path reply


clearReply : String -> Model -> Model
clearReply path =
    updateQuestionnaire <| QuestionnaireDetail.clearReplyValue path


setLabels : String -> List String -> Model -> Model
setLabels path value =
    updateQuestionnaire <| QuestionnaireDetail.setLabels path value


updateQuestionnaire : (QuestionnaireDetail -> QuestionnaireDetail) -> Model -> Model
updateQuestionnaire fn model =
    { model | questionnaire = fn model.questionnaire }


type alias Config msg =
    { features : FeaturesConfig
    , renderer : QuestionnaireRenderer Msg
    , wrapMsg : Msg -> msg
    , previewQuestionnaireEventMsg : Maybe (Uuid -> msg)
    , revertQuestionnaireMsg : Maybe (QuestionnaireEvent -> msg)
    }


type alias FeaturesConfig =
    { feedbackEnabled : Bool
    , todosEnabled : Bool
    , readonly : Bool
    , toolbarEnabled : Bool
    }


type alias QuestionnaireRenderer msg =
    { renderQuestionLabel : Question -> Html msg
    , renderQuestionDescription : QuestionnaireViewSettings -> Question -> Html msg
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
    | SetRightPanel RightPanel
    | SetFullscreen Bool
    | ScrollToPath String
    | ShowTypeHints (List String) String String
    | HideTypeHints
    | TypeHintInput (List String) Reply
    | TypeHintDebounceMsg Debounce.Msg
    | TypeHintsLoaded (List String) (Result ApiError (List TypeHint))
    | FeedbackModalMsg FeedbackModal.Msg
    | SetLevel String
    | SetReply String Reply
    | ClearReply String
    | AddItem String (List String)
    | SetLabels String (List String)
    | ViewSettingsDropdownMsg Dropdown.State
    | SetViewSettings QuestionnaireViewSettings
    | HistoryMsg History.Msg
    | VersionModalMsg VersionModal.Msg
    | DeleteVersionModalMsg DeleteVersionModal.Msg
    | CreateNamedVersion Uuid
    | RenameVersion QuestionnaireVersion
    | DeleteVersion QuestionnaireVersion
    | AddQuestionnaireVersion QuestionnaireVersion
    | UpdateQuestionnaireVersion QuestionnaireVersion
    | DeleteQuestionnaireVersion QuestionnaireVersion


update : Msg -> (Msg -> msg) -> Maybe (Bool -> msg) -> AppState -> Context -> Model -> ( Seed, Model, Cmd msg )
update msg wrapMsg mbSetFullscreenMsg appState ctx model =
    let
        withSeed ( newModel, cmd ) =
            ( appState.seed, newModel, Cmd.map wrapMsg cmd )

        wrap newModel =
            ( appState.seed, newModel, Cmd.none )
    in
    case msg of
        SetActivePage activePage ->
            withSeed <| ( { model | activePage = activePage }, Ports.scrollToTop ".questionnaire__content" )

        SetRightPanel rightPanel ->
            wrap { model | rightPanel = rightPanel }

        SetFullscreen fullscreen ->
            case mbSetFullscreenMsg of
                Just setFullscreenMsg ->
                    ( appState.seed, model, dispatch (setFullscreenMsg fullscreen) )

                Nothing ->
                    ( appState.seed, model, Cmd.none )

        ScrollToPath path ->
            withSeed <| handleScrollToPath model path

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
            handleAddItem appState wrapMsg model path originalItems

        SetLabels path value ->
            wrap <| setLabels path value model

        ViewSettingsDropdownMsg state ->
            wrap { model | viewSettingsDropdown = state }

        SetViewSettings viewSettings ->
            wrap { model | viewSettings = viewSettings }

        HistoryMsg historyMsg ->
            wrap { model | historyModel = History.update historyMsg model.historyModel }

        VersionModalMsg versionModalMsg ->
            let
                cfg =
                    { wrapMsg = VersionModalMsg
                    , questionnaireUuid = model.questionnaire.uuid
                    , addVersionCmd = dispatch << AddQuestionnaireVersion
                    , renameVersionCmd = dispatch << UpdateQuestionnaireVersion
                    }

                ( versionModalModel, cmd ) =
                    VersionModal.update cfg appState versionModalMsg model.versionModalModel

                newModel =
                    { model | versionModalModel = versionModalModel }
            in
            withSeed ( newModel, cmd )

        DeleteVersionModalMsg modalMsg ->
            let
                cfg =
                    { wrapMsg = DeleteVersionModalMsg
                    , questionnaireUuid = model.questionnaire.uuid
                    , deleteVersionCmd = dispatch << DeleteQuestionnaireVersion
                    }

                ( deleteVersionModalModel, cmd ) =
                    DeleteVersionModal.update cfg appState modalMsg model.deleteVersionModalModel

                newModel =
                    { model | deleteVersionModalModel = deleteVersionModalModel }
            in
            withSeed ( newModel, cmd )

        CreateNamedVersion eventUuid ->
            wrap { model | versionModalModel = VersionModal.setEventUuid eventUuid model.versionModalModel }

        RenameVersion questionnaireVersion ->
            wrap { model | versionModalModel = VersionModal.setVersion questionnaireVersion model.versionModalModel }

        DeleteVersion questionnaireVersion ->
            wrap { model | deleteVersionModalModel = DeleteVersionModal.setVersion questionnaireVersion model.deleteVersionModalModel }

        AddQuestionnaireVersion questionnaireVersion ->
            let
                questionnaire =
                    model.questionnaire
            in
            wrap { model | questionnaire = { questionnaire | versions = questionnaireVersion :: questionnaire.versions } }

        UpdateQuestionnaireVersion questionnaireVersion ->
            let
                questionnaire =
                    model.questionnaire

                updateVersion version =
                    if version.uuid == questionnaireVersion.uuid then
                        { version | name = questionnaireVersion.name, description = questionnaireVersion.description }

                    else
                        version
            in
            wrap { model | questionnaire = { questionnaire | versions = List.map updateVersion questionnaire.versions } }

        DeleteQuestionnaireVersion questionnaireVersion ->
            let
                questionnaire =
                    model.questionnaire
            in
            wrap
                { model
                    | questionnaire =
                        { questionnaire
                            | versions = List.filter (not << (==) questionnaireVersion.uuid << .uuid) questionnaire.versions
                        }
                }


handleScrollToPath : Model -> String -> ( Model, Cmd Msg )
handleScrollToPath model path =
    let
        chapterUuid =
            String.split "." path
                |> List.head
                |> Maybe.withDefault ""

        selector =
            "[data-path=\"" ++ path ++ "\"]"
    in
    ( { model | activePage = PageChapter chapterUuid }, Ports.scrollIntoView selector )


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


handleTypeHintsInput : Model -> List String -> Reply -> ( Model, Cmd Msg )
handleTypeHintsInput model path reply =
    let
        questionUuid =
            Maybe.withDefault "" (List.last path)

        ( debounce, debounceCmd ) =
            Debounce.push
                debounceConfig
                ( path, questionUuid, ReplyValue.getStringReply reply.value )
                model.typeHintsDebounce

        dispatchCmd =
            dispatch <|
                SetReply (String.join "." path) reply
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


handleAddItem : AppState -> (Msg -> msg) -> Model -> String -> List String -> ( Seed, Model, Cmd msg )
handleAddItem appState wrapMsg model path originalItems =
    let
        ( uuid, newSeed ) =
            getUuidString appState.seed

        dispatchCmd =
            ItemListReply (originalItems ++ [ uuid ])
                |> createReply appState
                |> SetReply path
                |> wrapMsg
                |> dispatch
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.viewSettingsDropdown ViewSettingsDropdownMsg
        , Sub.map HistoryMsg <| History.subscriptions model.historyModel
        ]



-- VIEW


view : AppState -> Config msg -> Context -> Model -> Html msg
view appState cfg ctx model =
    let
        ( toolbar, toolbarEnabled ) =
            if cfg.features.toolbarEnabled && not cfg.features.readonly then
                ( Html.map cfg.wrapMsg <| viewQuestionnaireToolbar appState model, True )

            else
                ( emptyNode, False )
    in
    div [ class "questionnaire", classList [ ( "toolbar-enabled", toolbarEnabled ) ] ]
        [ toolbar
        , div [ class "questionnaire__body" ]
            [ Html.map cfg.wrapMsg <| viewQuestionnaireLeftPanel appState cfg ctx model
            , Html.map cfg.wrapMsg <| viewQuestionnaireContent appState cfg ctx model
            , viewQuestionnaireRightPanel appState cfg ctx model
            ]
        , Html.map (cfg.wrapMsg << FeedbackModalMsg) <| FeedbackModal.view appState model.feedbackModalModel
        ]



-- QUESTIONNAIRE - TOOLBAR


viewQuestionnaireToolbar : AppState -> Model -> Html Msg
viewQuestionnaireToolbar appState model =
    let
        ( todosPanel, todosOpen ) =
            case model.rightPanel of
                RightPanelTODOs ->
                    ( RightPanelNone, True )

                _ ->
                    ( RightPanelTODOs, False )

        ( versionsPanel, versionsOpen ) =
            case model.rightPanel of
                RightPanelHistory ->
                    ( RightPanelNone, True )

                _ ->
                    ( RightPanelHistory, False )

        todosLength =
            QuestionnaireDetail.todosLength model.questionnaire

        todosBadge =
            if todosLength > 0 then
                span [ class "badge badge-pill badge-danger" ] [ text (String.fromInt todosLength) ]

            else
                emptyNode

        ( expandIcon, expandMsg ) =
            if AppState.isFullscreen appState then
                ( faSet "questionnaire.shrink" appState, SetFullscreen False )

            else
                ( faSet "questionnaire.expand" appState, SetFullscreen True )

        dropdown =
            let
                viewSettings =
                    model.viewSettings

                settingsIcon enabled =
                    if enabled then
                        faSet "_global.success" appState

                    else
                        emptyNode
            in
            div [ class "item-group" ]
                [ Dropdown.dropdown model.viewSettingsDropdown
                    { options = []
                    , toggleMsg = ViewSettingsDropdownMsg
                    , toggleButton =
                        Dropdown.toggle [ Button.roleLink, Button.attrs [ class "item" ] ]
                            [ lx_ "toolbar.view" appState ]
                    , items =
                        [ Dropdown.anchorItem
                            [ onClick (SetViewSettings QuestionnaireViewSettings.all) ]
                            [ lx_ "toolbar.view.showAll" appState ]
                        , Dropdown.anchorItem
                            [ onClick (SetViewSettings QuestionnaireViewSettings.none) ]
                            [ lx_ "toolbar.view.hideAll" appState ]
                        , Dropdown.divider
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon", onClick (SetViewSettings (QuestionnaireViewSettings.toggleAnsweredBy viewSettings)) ]
                            [ settingsIcon viewSettings.answeredBy, lx_ "toolbar.view.answeredBy" appState ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.togglePhases viewSettings))
                            ]
                            [ settingsIcon viewSettings.phases, lx_ "toolbar.view.phases" appState ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.toggleTags viewSettings))
                            ]
                            [ settingsIcon viewSettings.tags, lx_ "toolbar.view.tags" appState ]
                        ]
                    }
                ]
    in
    div [ class "questionnaire__toolbar" ]
        [ div [ class "questionnaire__toolbar__left" ]
            [ dropdown
            ]
        , div [ class "questionnaire__toolbar__right" ]
            [ div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", todosOpen ) ], onClick (SetRightPanel todosPanel) ]
                    [ lx_ "toolbar.todos" appState
                    , todosBadge
                    ]
                ]
            , div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", versionsOpen ) ], onClick (SetRightPanel versionsPanel) ]
                    [ lx_ "toolbar.versionHistory" appState ]
                ]
            , div [ class "item-group" ]
                [ a [ class "item", onClick expandMsg ] [ expandIcon ]
                ]
            ]
        ]



-- QUESTIONNAIRE - LEFT PANEL


viewQuestionnaireLeftPanel : AppState -> Config msg -> Context -> Model -> Html Msg
viewQuestionnaireLeftPanel appState cfg ctx model =
    div [ class "questionnaire__left-panel" ]
        [ viewQuestionnaireLeftPanelPhaseSelection appState cfg ctx model
        , viewQuestionnaireLeftPanelChapters appState model
        ]



-- QUESTIONNAIRE - LEFT PANEL - PHASE SELECTION


viewQuestionnaireLeftPanelPhaseSelection : AppState -> Config msg -> Context -> Model -> Html Msg
viewQuestionnaireLeftPanelPhaseSelection appState cfg ctx model =
    if appState.config.questionnaire.levels.enabled then
        let
            selectAttrs =
                if cfg.features.readonly then
                    [ disabled True ]

                else
                    [ onInput SetLevel ]
        in
        div [ class "questionnaire__left-panel__phase" ]
            [ label [] [ lgx "questionnaire.currentPhase" appState ]
            , select (class "form-control" :: selectAttrs)
                (List.map (viewQuestionnaireLeftPanelPhaseSelectionOption model.questionnaire.level) ctx.levels)
            ]

    else
        emptyNode


viewQuestionnaireLeftPanelPhaseSelectionOption : Int -> Level -> Html Msg
viewQuestionnaireLeftPanelPhaseSelectionOption selectedLevel level =
    option [ value (fromInt level.level), selected (selectedLevel == level.level) ]
        [ text level.title ]



-- QUESTIONNAIRE - LEFT PANEL - CHAPTERS


viewQuestionnaireLeftPanelChapters : AppState -> Model -> Html Msg
viewQuestionnaireLeftPanelChapters appState model =
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
    div [ class "questionnaire__left-panel__chapters" ]
        [ strong [] [ lgx "chapters" appState ]
        , div [ class "nav nav-pills flex-column" ]
            (List.indexedMap (viewQuestionnaireLeftPanelChaptersChapter appState model mbActiveChapterUuid) chapters)
        ]


viewQuestionnaireLeftPanelChaptersChapter : AppState -> Model -> Maybe String -> Int -> Chapter -> Html Msg
viewQuestionnaireLeftPanelChaptersChapter appState model mbActiveChapterUuid order chapter =
    a
        [ class "nav-link"
        , classList [ ( "active", mbActiveChapterUuid == Just chapter.uuid ) ]
        , onClick (SetActivePage (PageChapter chapter.uuid))
        ]
        [ span [ class "chapter-number" ] [ text (Roman.toRomanNumber (order + 1) ++ ". ") ]
        , span [ class "chapter-name" ] [ text chapter.title ]
        , viewQuestionnaireLeftPanelChaptersChapterIndication appState model.questionnaire chapter
        ]


viewQuestionnaireLeftPanelChaptersChapterIndication : AppState -> QuestionnaireDetail -> Chapter -> Html Msg
viewQuestionnaireLeftPanelChaptersChapterIndication appState questionnaire chapter =
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



-- QUESTIONNAIRE - RIGHT PANEL


viewQuestionnaireRightPanel : AppState -> Config msg -> Context -> Model -> Html msg
viewQuestionnaireRightPanel appState cfg ctx model =
    let
        wrapPanel content =
            div [ class "questionnaire__right-panel" ]
                content
    in
    case model.rightPanel of
        RightPanelNone ->
            emptyNode

        RightPanelTODOs ->
            wrapPanel <|
                [ Html.map cfg.wrapMsg <| viewQuestionnaireRightPanelTodos appState model ]

        RightPanelHistory ->
            let
                historyCfg =
                    { questionnaire = model.questionnaire
                    , levels = ctx.levels
                    , wrapMsg = cfg.wrapMsg << HistoryMsg
                    , scrollMsg = cfg.wrapMsg << ScrollToPath
                    , createVersionMsg = cfg.wrapMsg << CreateNamedVersion
                    , renameVersionMsg = cfg.wrapMsg << RenameVersion
                    , deleteVersionMsg = cfg.wrapMsg << DeleteVersion
                    , previewQuestionnaireEventMsg = cfg.previewQuestionnaireEventMsg
                    , revertQuestionnaireMsg = cfg.revertQuestionnaireMsg
                    }
            in
            wrapPanel <|
                [ History.view appState historyCfg model.historyModel
                , Html.map (cfg.wrapMsg << VersionModalMsg) <| VersionModal.view appState model.versionModalModel
                , Html.map (cfg.wrapMsg << DeleteVersionModalMsg) <| DeleteVersionModal.view appState model.deleteVersionModalModel
                ]



-- QUESTIONNAIRE - RIGHT PANEL - TODOS


viewQuestionnaireRightPanelTodos : AppState -> Model -> Html Msg
viewQuestionnaireRightPanelTodos appState model =
    let
        todos =
            QuestionnaireDetail.getTodos model.questionnaire

        viewTodoGroup group =
            div []
                [ strong [] [ text group.chapter.title ]
                , ul [ class "fa-ul" ] (List.map viewTodo group.todos)
                ]

        viewTodo todo =
            li []
                [ span [ class "fa-li" ] [ fa "far fa-check-square" ]
                , a [ onClick (ScrollToPath todo.path) ] [ text <| Question.getTitle todo.question ]
                ]
    in
    if List.isEmpty todos then
        div [ class "todos todos-empty" ] <|
            [ illustratedMessage "feeling_happy" (l_ "todos.completed" appState) ]

    else
        div [ class "todos" ] <|
            List.map viewTodoGroup (QuestionnaireTodoGroup.groupTodos todos)



-- QUESTIONNAIRE -- CONTENT


viewQuestionnaireContent : AppState -> Config msg -> Context -> Model -> Html Msg
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


viewQuestionnaireContentChapter : AppState -> Config msg -> Context -> Model -> Chapter -> Html Msg
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


viewQuestion : AppState -> Config msg -> Context -> Model -> List String -> List String -> Int -> Question -> Html Msg
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
                    ( viewQuestionValue appState cfg model newPath question, [] )

                IntegrationQuestion _ _ ->
                    ( viewQuestionIntegration appState cfg model newPath question, [] )

                MultiChoiceQuestion _ _ ->
                    ( viewQuestionMultiChoice appState cfg model newPath question, [] )

        viewLabel =
            viewQuestionLabel appState cfg ctx model newPath newHumanIdentifiers question

        viewTags =
            if model.viewSettings.tags then
                let
                    tags =
                        Question.getTagUuids question
                            |> List.map (flip KnowledgeModel.getTag model.questionnaire.knowledgeModel)
                            |> listFilterJust
                            |> List.sortBy .name
                in
                Tag.viewList tags

            else
                emptyNode

        viewDescription =
            cfg.renderer.renderQuestionDescription model.viewSettings question

        mbReply =
            Dict.get (pathToString newPath) model.questionnaire.replies

        viewAnsweredBy =
            case ( mbReply, model.viewSettings.answeredBy && not (Question.isList question) ) of
                ( Just reply, True ) ->
                    let
                        userName =
                            case reply.createdBy of
                                Just userInfo ->
                                    User.fullName userInfo

                                Nothing ->
                                    l_ "question.answeredBy.anonymous" appState

                        readableTime =
                            TimeUtils.toReadableDateTime appState.timeZone reply.createdAt

                        timeDiff =
                            Time.inWordsWithConfig { withAffix = True } (TimeDistance.locale appState) reply.createdAt appState.currentTime

                        time =
                            span [ title readableTime ] [ text timeDiff ]
                    in
                    div [ class "answered" ]
                        (lh_ "question.answeredBy" [ time, text userName ] appState)

                _ ->
                    emptyNode

        content =
            viewLabel :: viewTags :: viewDescription :: viewInput :: viewAnsweredBy :: viewExtensions

        questionExtraClass =
            Maybe.withDefault "" (cfg.renderer.getQuestionExtraClass question)
    in
    div
        [ class ("form-group " ++ questionExtraClass)
        , id ("question-" ++ Question.getUuid question)
        , attribute "data-path" (pathToString newPath)
        ]
        content


viewQuestionLabel : AppState -> Config msg -> Context -> Model -> List String -> List String -> Question -> Html Msg
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


viewQuestionOptions : AppState -> Config msg -> Context -> Model -> List String -> List String -> Question -> ( Html Msg, List (Html Msg) )
viewQuestionOptions appState cfg ctx model path humanIdentifiers question =
    let
        answers =
            KnowledgeModel.getQuestionAnswers (Question.getUuid question) model.questionnaire.knowledgeModel

        selectedAnswerUuid =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.map (.value >> ReplyValue.getAnswerUuid)

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


viewQuestionOptionsClearButton : AppState -> Config msg -> List String -> Maybe Answer -> Html Msg
viewQuestionOptionsClearButton appState cfg path mbSelectedAnswer =
    if cfg.features.readonly || Maybe.isNothing mbSelectedAnswer then
        emptyNode

    else
        a [ class "clear-answer", onClick (ClearReply (pathToString path)) ]
            [ faSet "questionnaire.clearAnswer" appState
            , lx_ "answer.clear" appState
            ]


viewQuestionOptionsFollowUps : AppState -> Config msg -> Context -> Model -> List Answer -> List String -> List String -> Answer -> Html Msg
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


viewQuestionMultiChoice : AppState -> Config msg -> Model -> List String -> Question -> Html Msg
viewQuestionMultiChoice appState cfg model path question =
    let
        choices =
            KnowledgeModel.getQuestionChoices (Question.getUuid question) model.questionnaire.knowledgeModel

        selectedChoicesUuids =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap [] (.value >> ReplyValue.getChoiceUuid)
    in
    div [] (List.indexedMap (viewChoice appState cfg path selectedChoicesUuids) choices)


viewQuestionList : AppState -> Config msg -> Context -> Model -> List String -> List String -> Question -> Html Msg
viewQuestionList appState cfg ctx model path humanIdentifiers question =
    let
        viewItem =
            viewQuestionListItem appState cfg ctx model question itemUuids path humanIdentifiers

        itemUuids =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)

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


viewQuestionListAdd : AppState -> Config msg -> List String -> List String -> Html Msg
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


viewQuestionListItem : AppState -> Config msg -> Context -> Model -> Question -> List String -> List String -> List String -> Int -> String -> Html Msg
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
                    , onClick (SetReply (pathToString path) (createReply appState (ItemListReply newItems)))
                    ]
                    [ faSet "_global.delete" appState ]
    in
    div [ class "item" ]
        [ div [ class "card bg-light mb-5" ]
            [ div [ class "card-body" ] itemQuestions
            ]
        , deleteButton
        ]


viewQuestionValue : AppState -> Config msg -> Model -> List String -> Question -> Html Msg
viewQuestionValue appState cfg model path question =
    let
        answer =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap "" (.value >> ReplyValue.getStringReply)

        defaultAttrs =
            [ class "form-control", value answer ]

        extraAttrs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onInput (SetReply (pathToString path) << createReply appState << StringReply) ]

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


viewQuestionIntegration : AppState -> Config msg -> Model -> List String -> Question -> Html Msg
viewQuestionIntegration appState cfg model path question =
    let
        extraArgs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onInput (TypeHintInput path << createReply appState << IntegrationReply << PlainType)
                , onFocus (ShowTypeHints path (Question.getUuid question) questionValue)
                , onBlur HideTypeHints
                ]

        mbReplyValue =
            Maybe.map .value <|
                Dict.get (pathToString path) model.questionnaire.replies

        questionValue =
            Maybe.unwrap "" ReplyValue.getStringReply mbReplyValue

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
        , viewQuestionIntegrationReplyExtra integration mbReplyValue
        ]


viewQuestionIntegrationTypeHints : AppState -> Config msg -> Model -> List String -> Html Msg
viewQuestionIntegrationTypeHints appState cfg model path =
    let
        content =
            case Maybe.unwrap Unset .hints model.typeHints of
                Success hints ->
                    ul [] (List.map (viewQuestionIntegrationTypeHint appState cfg path) hints)

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


viewQuestionIntegrationTypeHint : AppState -> Config msg -> List String -> TypeHint -> Html Msg
viewQuestionIntegrationTypeHint appState cfg path typeHint =
    if cfg.features.readonly then
        emptyNode

    else
        li []
            [ a
                [ onMouseDown <| SetReply (pathToString path) <| createReply appState <| IntegrationReply <| IntegrationType typeHint.id typeHint.name ]
                [ text typeHint.name
                ]
            ]


viewQuestionIntegrationReplyExtra : Maybe Integration -> Maybe ReplyValue -> Html Msg
viewQuestionIntegrationReplyExtra mbIntegration mbReplyValue =
    case ( mbIntegration, mbReplyValue ) of
        ( Just integration, Just (IntegrationReply (IntegrationType id _)) ) ->
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


viewChoice : AppState -> Config msg -> List String -> List String -> Int -> Choice -> Html Msg
viewChoice appState cfg path selectedChoicesUuids order choice =
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
                [ onClick (SetReply (pathToString path) (createReply appState (MultiChoiceReply newSelectedUuids))) ]
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


viewAnswer : AppState -> Config msg -> List String -> Maybe String -> Int -> Answer -> Html Msg
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
                [ onClick (SetReply (pathToString path) (createReply appState (AnswerReply answer.uuid))) ]

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


viewTodoAction : AppState -> Config msg -> Model -> List String -> Html Msg
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


viewFeedbackAction : AppState -> Config msg -> Model -> Question -> Html Msg
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


createReply : AppState -> ReplyValue -> Reply
createReply appState value =
    { value = value
    , createdAt = appState.currentTime
    , createdBy = Maybe.map UserInfo.toUserSuggestion appState.session.user
    }
