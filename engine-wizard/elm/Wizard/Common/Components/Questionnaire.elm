module Wizard.Common.Components.Questionnaire exposing
    ( ActivePage(..)
    , Config
    , Context
    , FeaturesConfig
    , Model
    , Msg(..)
    , QuestionnaireRenderer
    , RightPanel
    , TypeHints
    , addComment
    , clearReply
    , deleteComment
    , deleteCommentThread
    , editComment
    , init
    , reopenCommentThread
    , resolveCommentThread
    , setActiveChapterUuid
    , setLabels
    , setPhaseUuid
    , setReply
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Browser.Events
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h2, i, img, input, label, li, option, p, select, span, strong, text, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, href, id, name, placeholder, selected, src, target, title, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onFocus, onInput, onMouseDown)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Regex
import Roman
import Shared.Api.TypeHints as TypeHintsApi
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.Event exposing (Event)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Integration exposing (Integration(..))
import Shared.Data.KnowledgeModel.Integration.ApiIntegrationData exposing (ApiIntegrationData)
import Shared.Data.KnowledgeModel.Integration.CommonIntegrationData exposing (CommonIntegrationData)
import Shared.Data.KnowledgeModel.Integration.WidgetIntegrationData exposing (WidgetIntegrationData)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.Comment exposing (Comment)
import Shared.Data.QuestionnaireDetail.CommentThread exposing (CommentThread)
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
import Shared.Locale exposing (l, lg, lgx, lh, lx)
import Shared.Markdown as Markdown
import Shared.RegexPatterns as RegexPatterns
import Shared.Undraw as Undraw
import Shared.Utils exposing (dispatch, flip, getUuidString, listFilterJust, listInsertIf)
import String exposing (fromInt)
import Time
import Time.Distance as Time
import Uuid exposing (Uuid)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.DatePicker as DatePicker
import Wizard.Common.Components.Questionnaire.DeleteVersionModal as DeleteVersionModal
import Wizard.Common.Components.Questionnaire.FeedbackModal as FeedbackModal
import Wizard.Common.Components.Questionnaire.History as History
import Wizard.Common.Components.Questionnaire.QuestionnaireViewSettings as QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Common.Components.Questionnaire.VersionModal as VersionModal
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (illustratedMessage, linkTo, resizableTextarea)
import Wizard.Common.Html.Attribute exposing (dataCy, grammarlyAttributes)
import Wizard.Common.IntegrationWidgetValue as IntegrationWidgetValue
import Wizard.Common.TimeDistance as TimeDistance
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Tag as Tag
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireTodoGroup as QuestionnaireTodoGroup
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Questionnaire"


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
    , removeItem : Maybe ( String, String )
    , typeHints : Maybe TypeHints
    , typeHintsDebounce : Debounce ( List String, String, String )
    , feedbackModalModel : FeedbackModal.Model
    , viewSettings : QuestionnaireViewSettings
    , viewSettingsDropdown : Dropdown.State
    , historyModel : History.Model
    , versionModalModel : VersionModal.Model
    , deleteVersionModalModel : DeleteVersionModal.Model
    , commentInputs : Dict String String
    , commentEditInputs : Dict String String
    , commentDeleting : Maybe Uuid
    , commentDeletingListenClicks : Bool
    , commentsViewResolved : Bool
    , commentsViewPrivate : Bool
    , commentDropdownStates : Dict String Dropdown.State
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
    | RightPanelCommentsOverview
    | RightPanelComments String
    | RightPanelWarnings


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
    , removeItem = Nothing
    , typeHints = Nothing
    , typeHintsDebounce = Debounce.init
    , feedbackModalModel = FeedbackModal.init
    , viewSettings = QuestionnaireViewSettings.all
    , viewSettingsDropdown = Dropdown.initialState
    , historyModel = History.init appState
    , versionModalModel = VersionModal.init
    , deleteVersionModalModel = DeleteVersionModal.init
    , commentInputs = Dict.empty
    , commentEditInputs = Dict.empty
    , commentDeleting = Nothing
    , commentDeletingListenClicks = False
    , commentsViewResolved = False
    , commentsViewPrivate = False
    , commentDropdownStates = Dict.empty
    }


setActiveChapterUuid : String -> Model -> Model
setActiveChapterUuid uuid model =
    { model | activePage = PageChapter uuid }


setPhaseUuid : Maybe Uuid -> Model -> Model
setPhaseUuid phaseUuid =
    updateQuestionnaire <| QuestionnaireDetail.setPhaseUuid phaseUuid


setReply : String -> Reply -> Model -> Model
setReply path reply =
    updateQuestionnaire <| QuestionnaireDetail.setReply path reply


clearReply : String -> Model -> Model
clearReply path =
    updateQuestionnaire <| QuestionnaireDetail.clearReplyValue path


setLabels : String -> List String -> Model -> Model
setLabels path value =
    updateQuestionnaire <| QuestionnaireDetail.setLabels path value


resolveCommentThread : String -> Uuid -> Model -> Model
resolveCommentThread path threadUuid =
    updateQuestionnaire <| QuestionnaireDetail.resolveCommentThread path threadUuid


reopenCommentThread : String -> Uuid -> Model -> Model
reopenCommentThread path threadUuid =
    updateQuestionnaire <| QuestionnaireDetail.reopenCommentThread path threadUuid


deleteCommentThread : String -> Uuid -> Model -> Model
deleteCommentThread path threadUuid =
    updateQuestionnaire <| QuestionnaireDetail.deleteCommentThread path threadUuid


addComment : String -> Uuid -> Bool -> Comment -> Model -> Model
addComment path threadUuid private comment =
    updateQuestionnaire <| QuestionnaireDetail.addComment path threadUuid private comment


editComment : String -> Uuid -> Uuid -> Time.Posix -> String -> Model -> Model
editComment path threadUuid commentUuid updatedAt newText =
    updateQuestionnaire <| QuestionnaireDetail.editComment path threadUuid commentUuid updatedAt newText


deleteComment : String -> Uuid -> Uuid -> Model -> Model
deleteComment path threadUuid commentUuid =
    updateQuestionnaire <| QuestionnaireDetail.deleteComment path threadUuid commentUuid


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
    , commentsEnabled : Bool
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
    { events : List Event
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
    | ShowTypeHints (List String) Bool String String
    | HideTypeHints
    | TypeHintInput (List String) Bool Reply
    | TypeHintDebounceMsg Debounce.Msg
    | TypeHintsLoaded (List String) (Result ApiError (List TypeHint))
    | FeedbackModalMsg FeedbackModal.Msg
    | SetPhase String
    | SetReply String Reply
    | ClearReply String
    | AddItem String (List String)
    | RemoveItem String String
    | RemoveItemConfirm
    | RemoveItemCancel
    | OpenIntegrationWidget String String
    | GotIntegrationWidgetValue E.Value
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
    | OpenComments String
    | CommentInput String (Maybe Uuid) String
    | CommentSubmit String (Maybe Uuid) String Bool
    | CommentDelete (Maybe Uuid)
    | CommentDeleteListenClicks
    | CommentDeleteSubmit String Uuid Uuid Bool
    | CommentEditInput Uuid String
    | CommentEditCancel Uuid
    | CommentEditSubmit String Uuid Uuid String Bool
    | CommentThreadDelete String Uuid Bool
    | CommentThreadResolve String Uuid Bool
    | CommentThreadReopen String Uuid Bool
    | CommentsViewResolved Bool
    | CommentsViewPrivate Bool
    | CommentDropdownMsg String Dropdown.State


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

        ShowTypeHints path emptySearch questionUuid value ->
            withSeed <| handleShowTypeHints appState ctx model path emptySearch questionUuid value

        HideTypeHints ->
            wrap { model | typeHints = Nothing }

        TypeHintInput path emptySearch value ->
            withSeed <| handleTypeHintsInput model path emptySearch value

        TypeHintDebounceMsg debounceMsg ->
            withSeed <| handleTypeHintDebounceMsg appState ctx model debounceMsg

        TypeHintsLoaded path result ->
            wrap <| handleTypeHintsLoaded appState model path result

        FeedbackModalMsg feedbackModalMsg ->
            withSeed <| handleFeedbackModalMsg appState model feedbackModalMsg

        SetPhase phaseUuid ->
            wrap <| setPhaseUuid (Uuid.fromString phaseUuid) model

        SetReply path replyValue ->
            wrap <| setReply path replyValue model

        ClearReply path ->
            wrap <| clearReply path model

        AddItem path originalItems ->
            handleAddItem appState wrapMsg model path originalItems

        RemoveItem path itemUuid ->
            wrap <| { model | removeItem = Just ( path, itemUuid ) }

        RemoveItemConfirm ->
            case model.removeItem of
                Just ( path, itemUuid ) ->
                    let
                        itemUuids =
                            Dict.get path model.questionnaire.replies
                                |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)

                        newItemUuids =
                            List.filter ((/=) itemUuid) itemUuids

                        replyValue =
                            createReply appState (ItemListReply newItemUuids)

                        setReplyMsg =
                            SetReply path replyValue
                    in
                    withSeed ( { model | removeItem = Nothing }, dispatch setReplyMsg )

                Nothing ->
                    wrap model

        RemoveItemCancel ->
            wrap <| { model | removeItem = Nothing }

        OpenIntegrationWidget path requestUrl ->
            let
                data =
                    E.object [ ( "path", E.string path ), ( "requestUrl", E.string requestUrl ) ]
            in
            withSeed ( model, Ports.openIntegrationWidget data )

        GotIntegrationWidgetValue data ->
            case D.decodeValue IntegrationWidgetValue.decoder data of
                Ok value ->
                    let
                        setReplyMsg =
                            SetReply value.path <|
                                createReply appState <|
                                    IntegrationReply <|
                                        IntegrationType value.id value.value
                    in
                    withSeed ( model, dispatch setReplyMsg )

                Err _ ->
                    wrap model

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

        OpenComments path ->
            withSeed <| handleScrollToPath { model | rightPanel = RightPanelComments path } path

        CommentInput path mbThreadUuid value ->
            let
                key =
                    path ++ "-" ++ Maybe.unwrap "0" Uuid.toString mbThreadUuid

                commentInputs =
                    Dict.insert key value model.commentInputs
            in
            wrap { model | commentInputs = commentInputs }

        CommentSubmit path mbThreadUuid _ _ ->
            let
                key =
                    path ++ "-" ++ Maybe.unwrap "0" Uuid.toString mbThreadUuid

                commentInputs =
                    Dict.remove key model.commentInputs
            in
            wrap { model | commentInputs = commentInputs }

        CommentEditInput commentUuid value ->
            let
                commentEditInputs =
                    Dict.insert (Uuid.toString commentUuid) value model.commentEditInputs
            in
            wrap { model | commentEditInputs = commentEditInputs }

        CommentEditCancel commentUuid ->
            let
                commentEditInputs =
                    Dict.remove (Uuid.toString commentUuid) model.commentEditInputs
            in
            wrap { model | commentEditInputs = commentEditInputs }

        CommentEditSubmit _ _ commentUuid _ _ ->
            let
                commentEditInputs =
                    Dict.remove (Uuid.toString commentUuid) model.commentEditInputs
            in
            wrap { model | commentEditInputs = commentEditInputs }

        CommentDelete mbCommentUuid ->
            wrap { model | commentDeleting = mbCommentUuid, commentDeletingListenClicks = False }

        CommentDeleteListenClicks ->
            wrap { model | commentDeletingListenClicks = True }

        CommentsViewResolved value ->
            wrap { model | commentsViewResolved = value }

        CommentsViewPrivate value ->
            wrap { model | commentsViewPrivate = value }

        CommentDropdownMsg commentUuid state ->
            wrap { model | commentDropdownStates = Dict.insert commentUuid state model.commentDropdownStates }

        _ ->
            wrap model


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


handleShowTypeHints : AppState -> Context -> Model -> List String -> Bool -> String -> String -> ( Model, Cmd Msg )
handleShowTypeHints appState ctx model path emptySearch questionUuid value =
    if not emptySearch && String.isEmpty value then
        ( model, Cmd.none )

    else
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


handleTypeHintsInput : Model -> List String -> Bool -> Reply -> ( Model, Cmd Msg )
handleTypeHintsInput model path emptySearch reply =
    let
        ( ( debounce, debounceCmd ), newTypeHints ) =
            case ( emptySearch, reply.value ) of
                ( False, IntegrationReply (PlainType "") ) ->
                    ( ( model.typeHintsDebounce, Cmd.none ), Nothing )

                _ ->
                    let
                        questionUuid =
                            Maybe.withDefault "" (List.last path)

                        updatedTypeHints =
                            case model.typeHints of
                                Just typehints ->
                                    Just typehints

                                Nothing ->
                                    Just
                                        { path = path
                                        , hints = Loading
                                        }
                    in
                    ( Debounce.push
                        debounceConfig
                        ( path, questionUuid, ReplyValue.getStringReply reply.value )
                        model.typeHintsDebounce
                    , updatedTypeHints
                    )

        dispatchCmd =
            dispatch <|
                SetReply (pathToString path) reply
    in
    ( { model | typeHintsDebounce = debounce, typeHints = newTypeHints }
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
    let
        commentDeleteSub =
            case ( model.commentDeleting, model.commentDeletingListenClicks ) of
                ( Just _, False ) ->
                    Browser.Events.onAnimationFrame (\_ -> CommentDeleteListenClicks)

                ( Just _, True ) ->
                    Browser.Events.onClick (D.succeed (CommentDelete Nothing))

                _ ->
                    Sub.none

        commentDropdownSubs =
            Dict.toList model.commentDropdownStates
                |> List.map (\( uuid, state ) -> Dropdown.subscriptions state (CommentDropdownMsg uuid))
    in
    Sub.batch
        ([ Dropdown.subscriptions model.viewSettingsDropdown ViewSettingsDropdownMsg
         , Sub.map HistoryMsg <| History.subscriptions model.historyModel
         , commentDeleteSub
         , Ports.gotIntegrationWidgetValue GotIntegrationWidgetValue
         ]
            ++ commentDropdownSubs
        )



-- VIEW


view : AppState -> Config msg -> Context -> Model -> Html msg
view appState cfg ctx model =
    let
        ( toolbar, toolbarEnabled ) =
            if cfg.features.toolbarEnabled then
                ( Html.map cfg.wrapMsg <| viewQuestionnaireToolbar appState model, True )

            else
                ( emptyNode, False )

        ( migrationWarning, migrationWarningEnabled ) =
            case model.questionnaire.migrationUuid of
                Just migrationUuid ->
                    ( div [ class "questionnaire__warning" ]
                        [ div [ class "alert alert-warning" ]
                            (lh_ "migrationWarning"
                                [ linkTo appState (Routes.projectsMigration migrationUuid) [] [ lx_ "migrationWarning.migration" appState ] ]
                                appState
                            )
                        ]
                    , True
                    )

                Nothing ->
                    ( emptyNode, False )
    in
    div [ class "questionnaire", classList [ ( "toolbar-enabled", toolbarEnabled ), ( "warning-enabled", migrationWarningEnabled ) ] ]
        [ toolbar
        , migrationWarning
        , div [ class "questionnaire__body" ]
            [ Html.map cfg.wrapMsg <| viewQuestionnaireLeftPanel appState cfg model
            , Html.map cfg.wrapMsg <| viewQuestionnaireContent appState cfg ctx model
            , viewQuestionnaireRightPanel appState cfg model
            ]
        , Html.map (cfg.wrapMsg << FeedbackModalMsg) <| FeedbackModal.view appState model.feedbackModalModel
        , Html.map cfg.wrapMsg <| viewRemoveItemModal appState model
        ]



-- QUESTIONNAIRE - TOOLBAR


viewQuestionnaireToolbar : AppState -> Model -> Html Msg
viewQuestionnaireToolbar appState model =
    let
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

        navButton buttonElement visibleCondition =
            if visibleCondition then
                buttonElement

            else
                emptyNode

        ( todosPanel, todosOpen ) =
            case model.rightPanel of
                RightPanelTODOs ->
                    ( RightPanelNone, True )

                _ ->
                    ( RightPanelTODOs, False )

        ( commentsOverviewPanel, commentsOverviewOpen ) =
            case model.rightPanel of
                RightPanelCommentsOverview ->
                    ( RightPanelNone, True )

                _ ->
                    ( RightPanelCommentsOverview, False )

        ( versionsPanel, versionsOpen ) =
            case model.rightPanel of
                RightPanelHistory ->
                    ( RightPanelNone, True )

                _ ->
                    ( RightPanelHistory, False )

        ( warningsPanel, warningsOpen ) =
            case model.rightPanel of
                RightPanelWarnings ->
                    ( RightPanelNone, True )

                _ ->
                    ( RightPanelWarnings, False )

        todosLength =
            QuestionnaireDetail.todosLength model.questionnaire

        todosBadge =
            if todosLength > 0 then
                span [ class "badge badge-pill badge-danger" ] [ text (String.fromInt todosLength) ]

            else
                emptyNode

        todosButtonVisible =
            Feature.projectTodos appState model.questionnaire

        todosButton =
            div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", todosOpen ) ], onClick (SetRightPanel todosPanel) ]
                    [ lx_ "toolbar.todos" appState
                    , todosBadge
                    ]
                ]

        commentsLength =
            QuestionnaireDetail.commentsLength model.questionnaire

        commentsBadge =
            if commentsLength > 0 then
                span [ class "badge badge-pill badge-secondary", dataCy "questionnaire_toolbar_comments_count" ] [ text (String.fromInt commentsLength) ]

            else
                emptyNode

        commentsOverviewButtonVisible =
            Feature.projectCommentAdd appState model.questionnaire

        commentsOverviewButton =
            div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", commentsOverviewOpen ) ], onClick (SetRightPanel commentsOverviewPanel) ]
                    [ lx_ "toolbar.comments" appState
                    , commentsBadge
                    ]
                ]

        warningsLength =
            QuestionnaireDetail.warningsLength model.questionnaire

        warningsButton =
            div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", warningsOpen ) ], onClick (SetRightPanel warningsPanel) ]
                    [ lx_ "toolbar.warnings" appState
                    , span [ class "badge badge-pill badge-danger" ] [ text (String.fromInt warningsLength) ]
                    ]
                ]

        warningsButtonVisible =
            warningsLength > 0 || warningsOpen

        versionHistoryButtonVisible =
            Feature.projectVersionHitory appState model.questionnaire

        versionHistoryButton =
            div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", versionsOpen ) ], onClick (SetRightPanel versionsPanel) ]
                    [ lx_ "toolbar.versionHistory" appState ]
                ]

        ( expandIcon, expandMsg ) =
            if AppState.isFullscreen appState then
                ( faSet "questionnaire.shrink" appState, SetFullscreen False )

            else
                ( faSet "questionnaire.expand" appState, SetFullscreen True )
    in
    div [ class "questionnaire__toolbar" ]
        [ div [ class "questionnaire__toolbar__left" ]
            [ dropdown
            ]
        , div [ class "questionnaire__toolbar__right" ]
            [ navButton warningsButton warningsButtonVisible
            , navButton commentsOverviewButton commentsOverviewButtonVisible
            , navButton todosButton todosButtonVisible
            , navButton versionHistoryButton versionHistoryButtonVisible
            , div [ class "item-group" ]
                [ a [ class "item", onClick expandMsg ] [ expandIcon ]
                ]
            ]
        ]



-- QUESTIONNAIRE - LEFT PANEL


viewQuestionnaireLeftPanel : AppState -> Config msg -> Model -> Html Msg
viewQuestionnaireLeftPanel appState cfg model =
    div [ class "questionnaire__left-panel" ]
        [ viewQuestionnaireLeftPanelPhaseSelection appState cfg model
        , viewQuestionnaireLeftPanelChapters appState model
        ]



-- QUESTIONNAIRE - LEFT PANEL - PHASE SELECTION


viewQuestionnaireLeftPanelPhaseSelection : AppState -> Config msg -> Model -> Html Msg
viewQuestionnaireLeftPanelPhaseSelection appState cfg model =
    let
        phases =
            KnowledgeModel.getPhases model.questionnaire.knowledgeModel
    in
    if List.length phases > 0 then
        let
            selectAttrs =
                if cfg.features.readonly then
                    [ disabled True ]

                else
                    [ onInput SetPhase ]
        in
        div [ class "questionnaire__left-panel__phase" ]
            [ label [] [ lgx "questionnaire.currentPhase" appState ]
            , select (class "form-control" :: selectAttrs)
                (List.map (viewQuestionnaireLeftPanelPhaseSelectionOption model.questionnaire.phaseUuid) phases)
            ]

    else
        emptyNode


viewQuestionnaireLeftPanelPhaseSelectionOption : Maybe Uuid -> Phase -> Html Msg
viewQuestionnaireLeftPanelPhaseSelectionOption mbSelectedPhaseUuid phase =
    option
        [ value phase.uuid
        , selected (Maybe.unwrap False (Uuid.toString >> (==) phase.uuid) mbSelectedPhaseUuid)
        ]
        [ text phase.title ]



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
        unanswered =
            QuestionnaireDetail.calculateUnansweredQuestionsForChapter appState questionnaire chapter
    in
    if unanswered > 0 then
        span [ class "badge badge-light badge-pill" ] [ text <| fromInt unanswered ]

    else
        faSet "questionnaire.answeredIndication" appState



-- QUESTIONNAIRE - RIGHT PANEL


viewQuestionnaireRightPanel : AppState -> Config msg -> Model -> Html msg
viewQuestionnaireRightPanel appState cfg model =
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

        RightPanelCommentsOverview ->
            wrapPanel <|
                [ Html.map cfg.wrapMsg <| viewQuestionnaireRightPanelCommentsOverview appState model ]

        RightPanelComments path ->
            wrapPanel <|
                [ Html.map cfg.wrapMsg <| viewQuestionnaireRightPanelComments appState model path ]

        RightPanelHistory ->
            let
                historyCfg =
                    { questionnaire = model.questionnaire
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

        RightPanelWarnings ->
            wrapPanel <|
                [ Html.map cfg.wrapMsg <| viewQuestionnaireRightPanelWarnings appState model ]



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
            [ illustratedMessage Undraw.feelingHappy (l_ "todos.completed" appState) ]

    else
        div [ class "todos" ] <|
            List.map viewTodoGroup (QuestionnaireTodoGroup.groupTodos todos)



-- QUESTIONNAIRE - RIGHT PANEL - WARNINGS


viewQuestionnaireRightPanelWarnings : AppState -> Model -> Html Msg
viewQuestionnaireRightPanelWarnings appState model =
    let
        warnings =
            QuestionnaireDetail.getWarnings model.questionnaire

        viewWarningGroup group =
            div []
                [ strong [] [ text group.chapter.title ]
                , ul [ class "fa-ul" ] (List.map viewTodo group.todos)
                ]

        viewTodo todo =
            li []
                [ span [ class "fa-li" ] [ fa "fas fa-exclamation-triangle" ]
                , a [ onClick (ScrollToPath todo.path) ] [ text <| Question.getTitle todo.question ]
                ]
    in
    if List.isEmpty warnings then
        div [ class "todos todos-empty" ] <|
            [ illustratedMessage Undraw.feelingHappy (l_ "warnings.noWarnings" appState) ]

    else
        div [ class "todos" ] <|
            List.map viewWarningGroup (QuestionnaireTodoGroup.groupTodos warnings)



-- QUESTIONNAIRE - RIGHT PANEL - COMMENTS OVERVIEW


viewQuestionnaireRightPanelCommentsOverview : AppState -> Model -> Html Msg
viewQuestionnaireRightPanelCommentsOverview appState model =
    let
        viewChapterComments group =
            div []
                [ strong [] [ text group.chapter.title ]
                , ul [] (List.map viewQuestionComments group.todos)
                ]

        viewQuestionComments comment =
            li []
                [ a [ onClick (OpenComments comment.path) ]
                    [ span [ class "question" ] [ text <| Question.getTitle comment.question ]
                    , span [ class "badge badge-pill badge-light" ] [ text (String.fromInt comment.comments) ]
                    ]
                ]

        questionnaireComments =
            QuestionnaireDetail.getComments model.questionnaire
                |> groupComments
                |> List.map viewChapterComments

        groupComments comments =
            let
                fold comment acc =
                    if List.any (\group -> group.chapter.uuid == comment.chapter.uuid) acc then
                        List.map
                            (\group ->
                                if group.chapter.uuid == comment.chapter.uuid then
                                    { group | todos = group.todos ++ [ comment ] }

                                else
                                    group
                            )
                            acc

                    else
                        acc ++ [ { chapter = comment.chapter, todos = [ comment ] } ]
            in
            List.foldl fold [] comments

        content =
            if List.isEmpty questionnaireComments then
                [ div [ class "alert alert-info" ]
                    [ p
                        []
                        (lh_ "commentsOverview.empty" [ faSet "questionnaire.comments" appState ] appState)
                    ]
                ]

            else
                questionnaireComments
    in
    div [ class "comments-overview" ] content



-- QUESTIONNAIRE - RIGHT PANEL - COMMENTS


viewQuestionnaireRightPanelComments : AppState -> Model -> String -> Html Msg
viewQuestionnaireRightPanelComments appState model path =
    let
        commentThreads =
            Dict.get path model.questionnaire.commentThreadsMap
                |> Maybe.withDefault []

        navigationView =
            if Feature.projectCommentPrivate appState model.questionnaire then
                viewCommentsNavigation appState model commentThreads

            else
                emptyNode

        editorNoteExplanation =
            if model.commentsViewPrivate then
                div [ class "alert alert-editor-notes" ]
                    [ i [ class "fa fas fa-lock" ] []
                    , span [] [ lx_ "comments.editorNotes.description" appState ]
                    ]

            else
                emptyNode

        viewFilteredCommentThreads condition =
            commentThreads
                |> List.filter (\thread -> thread.private == model.commentsViewPrivate)
                |> List.filter condition
                |> List.map (viewCommentThread appState model path)

        resolvedSelect =
            div [ class "form-check" ]
                [ label [ class "form-check-label form-check-toggle" ]
                    [ input [ type_ "checkbox", class "form-check-input", onCheck CommentsViewResolved ] []
                    , span [] [ lx_ "comments.viewResolved" appState ]
                    ]
                ]

        resolvedThreadsView =
            if model.commentsViewResolved then
                div [] (viewFilteredCommentThreads (\thread -> thread.resolved))

            else
                emptyNode

        commentThreadsView =
            div [] (viewFilteredCommentThreads (\thread -> not thread.resolved))

        newThreadForm =
            viewCommentReplyForm appState
                { submitText = l_ "comments.newThread.submit" appState
                , placeholderText = l_ "comments.newThread.placeholder" appState
                , model = model
                , path = path
                , mbThreadUuid = Nothing
                , private = model.commentsViewPrivate
                }
    in
    div [ class "Comments" ]
        [ resolvedSelect
        , navigationView
        , resolvedThreadsView
        , commentThreadsView
        , editorNoteExplanation
        , newThreadForm
        ]


viewCommentsNavigation : AppState -> Model -> List CommentThread -> Html Msg
viewCommentsNavigation appState model commentThreads =
    let
        threadCount predicate =
            List.filter predicate commentThreads
                |> List.filter (not << .resolved)
                |> List.map (.comments >> List.length)
                |> List.sum

        publicThreadsCount =
            threadCount (not << .private)

        privateThreadsCount =
            threadCount .private

        toBadge count =
            if count == 0 then
                emptyNode

            else
                span [ class "badge badge-pill badge-light" ] [ text (String.fromInt count) ]
    in
    ul [ class "nav nav-underline-tabs" ]
        [ li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", not model.commentsViewPrivate ) ]
                , onClick (CommentsViewPrivate False)
                , dataCy "comments_nav_comments"
                ]
                [ span [ attribute "data-content" (l_ "comments.nav.comments" appState) ]
                    [ lx_ "comments.nav.comments" appState ]
                , toBadge publicThreadsCount
                ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ class "nav-link nav-link-editor-notes"
                , classList [ ( "active", model.commentsViewPrivate ) ]
                , onClick (CommentsViewPrivate True)
                , dataCy "comments_nav_private-notes"
                ]
                [ span [ attribute "data-content" (l_ "comments.nav.editorNotes" appState) ]
                    [ lx_ "comments.nav.editorNotes" appState ]
                , toBadge privateThreadsCount
                ]
            ]
        ]


viewCommentThread : AppState -> Model -> String -> CommentThread -> Html Msg
viewCommentThread appState model path commentThread =
    let
        deleteOverlay =
            if model.commentDeleting == Maybe.map .uuid (List.head commentThread.comments) then
                viewCommentDeleteOverlay appState
                    { deleteMsg = CommentThreadDelete path commentThread.uuid commentThread.private
                    , deleteText = l_ "comments.commentThread.delete" appState
                    , extraClass = "CommentDeleteOverlay--Thread"
                    }

            else
                emptyNode

        replyForm =
            if commentThread.resolved then
                emptyNode

            else
                viewCommentReplyForm appState
                    { submitText = l_ "comments.reply.submit" appState
                    , placeholderText = l_ "comments.reply.placeholder" appState
                    , model = model
                    , path = path
                    , mbThreadUuid = Just commentThread.uuid
                    , private = commentThread.private
                    }
    in
    div
        [ class "CommentThread"
        , classList
            [ ( "CommentThread--Resolved", commentThread.resolved )
            , ( "CommentThread--Private", commentThread.private )
            ]
        ]
        (List.indexedMap (viewComment appState model path commentThread) commentThread.comments
            ++ [ replyForm, deleteOverlay ]
        )


viewComment : AppState -> Model -> String -> CommentThread -> Int -> Comment -> Html Msg
viewComment appState model path commentThread index comment =
    let
        commentHeader =
            viewCommentHeader appState model path commentThread index comment

        mbEditValue =
            Dict.get (Uuid.toString comment.uuid) model.commentEditInputs

        content =
            case mbEditValue of
                Just editValue ->
                    div []
                        [ resizableTextarea 2
                            editValue
                            [ class "form-control mb-1", onInput (CommentEditInput comment.uuid) ]
                            []
                        , div []
                            [ button
                                [ class "btn btn-primary btn-sm mr-1"
                                , disabled (String.isEmpty editValue)
                                , onClick (CommentEditSubmit path commentThread.uuid comment.uuid editValue commentThread.private)
                                ]
                                [ lx_ "comments.edit.submit" appState ]
                            , button
                                [ class "btn btn-outline-secondary btn-sm"
                                , onClick (CommentEditCancel comment.uuid)
                                ]
                                [ lx_ "comments.form.cancel" appState ]
                            ]
                        ]

                Nothing ->
                    div [] [ Markdown.toHtml [ class "Comment_MD" ] comment.text ]

        deleteOverlay =
            if index /= 0 && model.commentDeleting == Just comment.uuid then
                viewCommentDeleteOverlay appState
                    { deleteMsg = CommentDeleteSubmit path commentThread.uuid comment.uuid commentThread.private
                    , deleteText = l_ "comments.comment.delete" appState
                    , extraClass = "CommentDeleteOverlay--Comment"
                    }

            else
                emptyNode
    in
    div [ class "Comment" ]
        [ commentHeader
        , content
        , deleteOverlay
        ]


viewCommentHeader : AppState -> Model -> String -> CommentThread -> Int -> Comment -> Html Msg
viewCommentHeader appState model path commentThread index comment =
    let
        resolveAction =
            if index == 0 && Feature.projectCommentThreadResolve appState model.questionnaire commentThread then
                a
                    [ class "ml-1"
                    , onClick (CommentThreadResolve path commentThread.uuid commentThread.private)
                    , title (l_ "comments.comment.action.resolveTitle" appState)
                    , dataCy "comments_comment_resolve"
                    ]
                    [ faSet "questionnaire.commentsResolve" appState ]

            else
                emptyNode

        reopenAction =
            Dropdown.anchorItem
                [ onClick (CommentThreadReopen path commentThread.uuid commentThread.private) ]
                [ lx_ "comments.comment.action.reopen" appState ]

        reopenActionVisible =
            index == 0 && Feature.projectCommentThreadReopen appState model.questionnaire commentThread

        editAction =
            Dropdown.anchorItem
                [ onClick (CommentEditInput comment.uuid comment.text) ]
                [ lx_ "comments.comment.action.edit" appState ]

        editActionVisible =
            Feature.projectCommentEdit appState model.questionnaire commentThread comment

        deleteAction =
            Dropdown.anchorItem
                [ onClick (CommentDelete (Just comment.uuid))
                , dataCy "comments_comment_menu_delete"
                ]
                [ lx_ "comments.comment.action.delete" appState ]

        deleteActionVisible =
            (index == 0 && Feature.projectCommentThreadDelete appState model.questionnaire commentThread)
                || (index /= 0 && Feature.projectCommentDelete appState model.questionnaire commentThread comment)

        actions =
            []
                |> listInsertIf reopenAction reopenActionVisible
                |> listInsertIf editAction editActionVisible
                |> listInsertIf deleteAction deleteActionVisible

        dropdown =
            if List.isEmpty actions then
                emptyNode

            else
                let
                    dropdownState =
                        Dict.get (Uuid.toString comment.uuid) model.commentDropdownStates
                            |> Maybe.withDefault Dropdown.initialState
                in
                Dropdown.dropdown dropdownState
                    { options = [ Dropdown.attrs [ class "ListingDropdown", dataCy "comments_comment_menu" ], Dropdown.alignMenuRight ]
                    , toggleMsg = CommentDropdownMsg (Uuid.toString comment.uuid)
                    , toggleButton =
                        Dropdown.toggle [ Button.roleLink ]
                            [ faSet "listing.actions" appState ]
                    , items = actions
                    }

        createdLabel =
            TimeUtils.toReadableDateTime appState.timeZone comment.createdAt

        editedLabel =
            if comment.createdAt /= comment.updatedAt then
                span [ title (TimeUtils.toReadableDateTime appState.timeZone comment.updatedAt) ]
                    [ text <| " (" ++ l_ "comments.comment.editedLabel" appState ++ ")" ]

            else
                emptyNode

        userForIcon =
            case comment.createdBy of
                Just createdBy ->
                    { gravatarHash = createdBy.gravatarHash
                    , imageUrl = createdBy.imageUrl
                    }

                Nothing ->
                    { gravatarHash = ""
                    , imageUrl = Nothing
                    }
    in
    div [ class "Comment__Header" ]
        [ UserIcon.view userForIcon
        , div [ class "Comment__Header__User" ]
            [ strong [ class "Comment__Header__User__Name" ]
                [ text (Maybe.unwrap (lg "user.anonymous" appState) User.fullName comment.createdBy)
                ]
            , span [ class "Comment__Header__User__Time" ] [ text createdLabel, editedLabel ]
            ]
        , resolveAction
        , dropdown
        ]


viewCommentReplyForm :
    AppState
    ->
        { submitText : String
        , placeholderText : String
        , model : Model
        , path : String
        , mbThreadUuid : Maybe Uuid
        , private : Bool
        }
    -> Html Msg
viewCommentReplyForm appState { submitText, placeholderText, model, path, mbThreadUuid, private } =
    let
        commentValue =
            model.commentInputs
                |> Dict.get (path ++ "-" ++ Maybe.unwrap "0" Uuid.toString mbThreadUuid)
                |> Maybe.withDefault ""

        cyFormType base =
            let
                privateType =
                    if private then
                        "private"

                    else
                        "public"
            in
            case mbThreadUuid of
                Just _ ->
                    base ++ "_reply_" ++ privateType

                Nothing ->
                    base ++ "_new_" ++ privateType

        newThreadFormSubmit =
            if String.isEmpty commentValue then
                emptyNode

            else
                div []
                    [ button
                        [ class "btn btn-primary btn-sm mr-1"
                        , onClick (CommentSubmit path mbThreadUuid commentValue private)
                        , dataCy (cyFormType "comments_reply-form_submit")
                        ]
                        [ text submitText ]
                    , button
                        [ class "btn btn-outline-secondary btn-sm"
                        , onClick (CommentInput path mbThreadUuid "")
                        , dataCy (cyFormType "comments_reply-form_cancel")
                        ]
                        [ lx_ "comments.form.cancel" appState ]
                    ]
    in
    div [ class "CommentReplyForm", classList [ ( "CommentReplyForm--Private", private ) ] ]
        [ resizableTextarea 2
            commentValue
            [ class "form-control"
            , placeholder placeholderText
            , onInput (CommentInput path mbThreadUuid)
            , dataCy (cyFormType "comments_reply-form_input")
            ]
            []
        , newThreadFormSubmit
        ]


viewCommentDeleteOverlay : AppState -> { deleteMsg : Msg, deleteText : String, extraClass : String } -> Html Msg
viewCommentDeleteOverlay appState { deleteMsg, deleteText, extraClass } =
    div [ class "CommentDeleteOverlay", class extraClass ]
        [ div [ class "text-center" ]
            [ div [ class "mb-2" ] [ text deleteText ]
            , button
                [ class "btn btn-danger btn-sm mr-2"
                , onClick deleteMsg
                , dataCy "comments_delete-modal_delete"
                ]
                [ lx_ "comments.deleteOverlay.delete" appState ]
            , button [ class "btn btn-secondary btn-sm", onClick (CommentDelete Nothing) ] [ lx_ "comments.form.cancel" appState ]
            ]
        ]



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

                IntegrationQuestion _ data ->
                    let
                        mbIntegration =
                            KnowledgeModel.getIntegration data.integrationUuid model.questionnaire.knowledgeModel
                    in
                    case mbIntegration of
                        Just (ApiIntegration commonIntegrationData apiIntegrationData) ->
                            ( viewQuestionIntegrationApi appState cfg model newPath commonIntegrationData apiIntegrationData question, [] )

                        Just (WidgetIntegration commonIntegrationData widgetIntegrationData) ->
                            ( viewQuestionIntegrationWidget appState cfg model newPath commonIntegrationData widgetIntegrationData, [] )

                        _ ->
                            ( emptyNode, [] )

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
viewQuestionLabel appState cfg _ model path humanIdentifiers question =
    let
        isDesirable =
            Question.isDesirable
                model.questionnaire.knowledgeModel.phaseUuids
                (Uuid.toString <| Maybe.withDefault Uuid.nil model.questionnaire.phaseUuid)
                question

        questionState =
            case
                ( QuestionnaireDetail.hasReply (pathToString path) model.questionnaire
                , isDesirable
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
            , viewCommentAction appState cfg model path
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
            viewQuestionClearButton appState cfg path mbSelectedAnswer

        advice =
            Maybe.unwrap emptyNode cfg.renderer.renderAnswerAdvice mbSelectedAnswer

        followUps =
            Maybe.unwrap emptyNode
                (viewQuestionOptionsFollowUps appState cfg ctx model answers path humanIdentifiers)
                mbSelectedAnswer
    in
    ( div []
        (List.indexedMap (viewAnswer appState cfg model.questionnaire.knowledgeModel path selectedAnswerUuid) answers
            ++ [ clearReplyButton ]
        )
    , [ advice, followUps ]
    )


viewQuestionClearButton : AppState -> Config msg -> List String -> Maybe a -> Html Msg
viewQuestionClearButton appState cfg path mbSelectedAnswer =
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
            viewQuestionListItem appState cfg ctx model question path humanIdentifiers

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


viewQuestionListItem : AppState -> Config msg -> Context -> Model -> Question -> List String -> List String -> Int -> String -> Html Msg
viewQuestionListItem appState cfg ctx model question path humanIdentifiers index uuid =
    let
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
                    , onClick (RemoveItem (pathToString path) uuid)
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
        defaultValue =
            if Question.getValueType question == Just ColorQuestionValueType then
                "#000000"

            else
                ""

        answer =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap defaultValue (.value >> ReplyValue.getStringReply)

        defaultAttrs =
            [ class "form-control", value answer ]

        toMsg =
            SetReply (pathToString path) << createReply appState << StringReply

        extraAttrs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onInput toMsg ]

        warningView regex warning =
            if not (String.isEmpty answer) && not (Regex.contains regex answer) then
                Flash.warning appState warning

            else
                emptyNode

        defaultInput =
            [ input (type_ "text" :: defaultAttrs ++ extraAttrs) [] ]

        readonlyOr otherInput =
            if cfg.features.readonly then
                defaultInput

            else
                otherInput

        inputView =
            case Question.getValueType question of
                Just NumberQuestionValueType ->
                    [ input (type_ "number" :: defaultAttrs ++ extraAttrs) [] ]

                Just DateQuestionValueType ->
                    readonlyOr [ DatePicker.datePicker [ DatePicker.onChange toMsg, DatePicker.value answer ] ]

                Just DateTimeQuestionValueType ->
                    readonlyOr [ DatePicker.dateTimePicker [ DatePicker.onChange toMsg, DatePicker.value answer ] ]

                Just TimeQuestionValueType ->
                    readonlyOr [ DatePicker.timePicker [ DatePicker.onChange toMsg, DatePicker.value answer ] ]

                Just EmailQuestionValueType ->
                    [ input (type_ "email" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.email (l_ "value.invalidEmail" appState)
                    ]

                Just UrlQuestionValueType ->
                    [ input (type_ "email" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.url (l_ "value.invalidUrl" appState)
                    ]

                Just TextQuestionValueType ->
                    [ resizableTextarea 3 answer (defaultAttrs ++ extraAttrs ++ grammarlyAttributes) [] ]

                Just ColorQuestionValueType ->
                    [ input (type_ "color" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.color (l_ "value.invalidColor" appState)
                    ]

                _ ->
                    defaultInput
    in
    div [] inputView


viewQuestionIntegrationWidget : AppState -> Config msg -> Model -> List String -> CommonIntegrationData -> WidgetIntegrationData -> Html Msg
viewQuestionIntegrationWidget appState cfg model path commonIntegrationData widgetIntegrationData =
    let
        mbReplyValue =
            Maybe.map .value <|
                Dict.get (pathToString path) model.questionnaire.replies

        questionInput =
            case mbReplyValue of
                Just (IntegrationReply (IntegrationType id integrationValue)) ->
                    viewQuestionIntegrationIntegrationReply commonIntegrationData id integrationValue

                _ ->
                    viewQuestionIntegrationWidgetSelectButton appState cfg path widgetIntegrationData mbReplyValue
    in
    div [ class "question-integration-answer" ]
        [ questionInput
        , viewQuestionClearButton appState cfg path mbReplyValue
        ]


viewQuestionIntegrationWidgetSelectButton : AppState -> Config msg -> List String -> WidgetIntegrationData -> Maybe ReplyValue -> Html Msg
viewQuestionIntegrationWidgetSelectButton appState cfg path widgetIntegrationData mbReplyValue =
    case ( cfg.features.readonly, Maybe.isJust mbReplyValue ) of
        ( False, False ) ->
            button
                [ onClick (OpenIntegrationWidget (pathToString path) widgetIntegrationData.widgetUrl)
                , class "btn btn-secondary"
                ]
                [ lx_ "integrationWidget.select" appState ]

        _ ->
            emptyNode


viewQuestionIntegrationApi : AppState -> Config msg -> Model -> List String -> CommonIntegrationData -> ApiIntegrationData -> Question -> Html Msg
viewQuestionIntegrationApi appState cfg model path commonIntegrationData apiIntegrationData question =
    let
        extraArgs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                let
                    questionValue =
                        Maybe.unwrap "" ReplyValue.getStringReply mbReplyValue

                    onFocusHandler =
                        [ onFocus (ShowTypeHints path apiIntegrationData.requestEmptySearch (Question.getUuid question) questionValue) ]
                in
                [ onInput (TypeHintInput path apiIntegrationData.requestEmptySearch << createReply appState << IntegrationReply << PlainType)
                , onBlur HideTypeHints
                ]
                    ++ onFocusHandler

        mbReplyValue =
            Maybe.map .value <|
                Dict.get (pathToString path) model.questionnaire.replies

        viewInput currentValue =
            input ([ class "form-control", type_ "text", value currentValue ] ++ extraArgs) []

        questionInput =
            case mbReplyValue of
                Just (IntegrationReply integrationReply) ->
                    case integrationReply of
                        PlainType plainValue ->
                            viewInput plainValue

                        IntegrationType id integrationValue ->
                            viewQuestionIntegrationIntegrationReply commonIntegrationData id integrationValue

                _ ->
                    viewInput ""

        typeHintsVisible =
            Maybe.unwrap False (.path >> (==) path) model.typeHints

        viewTypeHints =
            if typeHintsVisible then
                viewQuestionIntegrationTypeHints appState cfg model path

            else
                emptyNode
    in
    div [ class "question-integration-answer" ]
        [ questionInput
        , viewTypeHints
        , viewQuestionClearButton appState cfg path mbReplyValue
        ]


viewQuestionIntegrationTypeHints : AppState -> Config msg -> Model -> List String -> Html Msg
viewQuestionIntegrationTypeHints appState cfg model path =
    let
        content =
            case Maybe.unwrap Unset .hints model.typeHints of
                Success [] ->
                    div [ class "info" ]
                        [ faSet "_global.info" appState
                        , lx_ "typeHints.empty" appState
                        ]

                Success hints ->
                    ul [ class "integration-typehints-list" ] (List.map (viewQuestionIntegrationTypeHint appState cfg path) hints)

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
    div [ class "integration-typehints" ] [ content ]


viewQuestionIntegrationTypeHint : AppState -> Config msg -> List String -> TypeHint -> Html Msg
viewQuestionIntegrationTypeHint appState cfg path typeHint =
    if cfg.features.readonly then
        emptyNode

    else
        li
            [ class "integration-typehints-list-item"
            , onMouseDown <| SetReply (pathToString path) <| createReply appState <| IntegrationReply <| IntegrationType typeHint.id typeHint.name
            ]
            [ Markdown.toHtml [ class "item-md" ] typeHint.name
            ]


viewQuestionIntegrationIntegrationReply : CommonIntegrationData -> String -> String -> Html Msg
viewQuestionIntegrationIntegrationReply integration id value =
    div [ class "card" ]
        [ Markdown.toHtml [ class "card-body item-md" ] value
        , viewQuestionIntegrationLink integration id
        ]


viewQuestionIntegrationLink : CommonIntegrationData -> String -> Html Msg
viewQuestionIntegrationLink integration id =
    let
        url =
            String.replace "${id}" id integration.itemUrl

        logo =
            if String.isEmpty integration.logo then
                emptyNode

            else
                img [ src integration.logo ] []
    in
    div [ class "card-footer" ]
        [ logo
        , a [ href url, target "_blank" ] [ text url ]
        ]


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


viewAnswer : AppState -> Config msg -> KnowledgeModel -> List String -> Maybe String -> Int -> Answer -> Html Msg
viewAnswer appState cfg km path selectedAnswerUuid order answer =
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
            if List.isEmpty (KnowledgeModel.getAnswerFollowupQuestions answer.uuid km) then
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


viewCommentAction : AppState -> Config msg -> Model -> List String -> Html Msg
viewCommentAction appState cfg model path =
    if cfg.features.commentsEnabled && Feature.projectCommentAdd appState model.questionnaire then
        let
            pathString =
                pathToString path

            commentCount =
                QuestionnaireDetail.getCommentCount pathString model.questionnaire

            isOpen =
                case model.rightPanel of
                    RightPanelComments rightPanelPath ->
                        rightPanelPath == pathString

                    _ ->
                        False

            msg =
                if isOpen then
                    SetRightPanel RightPanelNone

                else
                    SetRightPanel (RightPanelComments pathString)
        in
        if commentCount > 0 then
            a
                [ class "action action-comments"
                , classList [ ( "action-comments-open", isOpen ) ]
                , onClick msg
                , dataCy "questionnaire_question-action_comment"
                ]
                [ faSet "questionnaire.comments" appState
                , text <| String.fromInt commentCount ++ " comments"
                ]

        else
            a
                [ class "action"
                , classList [ ( "action-comments-open", isOpen ) ]
                , onClick msg
                , dataCy "questionnaire_question-action_comment"
                ]
                [ faSet "questionnaire.comments" appState ]

    else
        emptyNode


viewTodoAction : AppState -> Config msg -> Model -> List String -> Html Msg
viewTodoAction appState cfg model path =
    if cfg.features.todosEnabled then
        let
            currentPath =
                pathToString path

            hasTodo =
                model.questionnaire.labels
                    |> Dict.get currentPath
                    |> Maybe.unwrap False (List.member QuestionnaireDetail.todoUuid)
        in
        if hasTodo then
            span [ class "action action-todo" ]
                [ span [] [ lx_ "todoAction.todo" appState ]
                , a
                    [ title <| l_ "todoAction.remove" appState
                    , onClick <| SetLabels currentPath []
                    ]
                    [ faSet "_global.remove" appState ]
                ]

        else
            a
                [ class "action action-add-todo"
                , onClick <| SetLabels currentPath [ QuestionnaireDetail.todoUuid ]
                ]
                [ faSet "_global.add" appState
                , span [] [ span [] [ lx_ "todoAction.add" appState ] ]
                ]

    else
        emptyNode


viewFeedbackAction : AppState -> Config msg -> Model -> Question -> Html Msg
viewFeedbackAction appState cfg model question =
    let
        feedbackEnabled =
            appState.config.questionnaire.feedback.enabled && cfg.features.feedbackEnabled
    in
    if feedbackEnabled then
        let
            openFeedbackModal =
                FeedbackModalMsg (FeedbackModal.OpenFeedback model.questionnaire.package.id (Question.getUuid question))
        in
        a
            [ class "action"
            , attribute "data-cy" "feedback"
            , onClick openFeedbackModal
            ]
            [ faSet "questionnaire.feedback" appState ]

    else
        emptyNode


viewRemoveItemModal : AppState -> Model -> Html Msg
viewRemoveItemModal appState model =
    let
        cfg =
            { modalTitle = l_ "removeItemModal.title" appState
            , modalContent = [ lx_ "removeItemModal.text" appState ]
            , visible = Maybe.isJust model.removeItem
            , actionResult = Unset
            , actionName = l_ "removeItemModal.action" appState
            , actionMsg = RemoveItemConfirm
            , cancelMsg = Just RemoveItemCancel
            , dangerous = True
            , dataCy = "remove-item"
            }
    in
    Modal.confirm appState cfg



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
