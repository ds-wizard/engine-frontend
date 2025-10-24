module Wizard.Pages.DocumentTemplateEditors.Editor.Components.Preview exposing
    ( Model
    , Msg
    , PreviewMode
    , UpdateConfig
    , ViewConfig
    , initialModel
    , loadPreviewMsg
    , setSelectedKmEditor
    , setSelectedQuestionnaire
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Api.ServerError as ServerError
import Common.Components.FontAwesome exposing (fa, faDownload)
import Common.Components.Page as Page
import Common.Components.Tooltip exposing (tooltip)
import Common.Components.TypeHintInput as TypeHintInput
import Common.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Common.Components.Undraw as Undraw
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Utils.ContentType as ContentType
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setFormatUuid, setKnowledgeModelEditorUuid, setQuestionnaireUuid, setSelected)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, iframe, option, p, pre, select, text)
import Html.Attributes exposing (class, classList, href, id, name, selected, src, target, value)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick, onInput)
import Html.Extra as Html
import Http
import Maybe.Extra as Maybe
import Process
import String.Format as String
import Task
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Wizard.Api.Models.DocumentTemplateDraftDetail as DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Api.Models.KnowledgeModelEditorSuggestion exposing (KnowledgeModelEditorSuggestion)
import Wizard.Api.Models.QuestionnaireSuggestion exposing (QuestionnaireSuggestion)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Routes as Routes



-- MODEL


type alias Model =
    { questionnaireHintInputModel : TypeHintInput.Model QuestionnaireSuggestion
    , kmEditorTypeHintInputModal : TypeHintInput.Model KnowledgeModelEditorSuggestion
    , urlResponse : ActionResult UrlResponse
    , mode : PreviewMode
    }


type PreviewMode
    = QuestionnaireMode
    | KMEditorMode


initialModel : Model
initialModel =
    { questionnaireHintInputModel = TypeHintInput.init "uuid"
    , kmEditorTypeHintInputModal = TypeHintInput.init "uuid"
    , urlResponse = ActionResult.Unset
    , mode = QuestionnaireMode
    }


setSelectedQuestionnaire : Maybe QuestionnaireSuggestion -> Model -> Model
setSelectedQuestionnaire questionnaire model =
    let
        newMode =
            if Maybe.isJust questionnaire then
                QuestionnaireMode

            else
                model.mode
    in
    { model
        | questionnaireHintInputModel = setSelected questionnaire model.questionnaireHintInputModel
        , mode = newMode
    }


setSelectedKmEditor : Maybe KnowledgeModelEditorSuggestion -> Model -> Model
setSelectedKmEditor kmEditor model =
    let
        newMode =
            if Maybe.isJust kmEditor then
                KMEditorMode

            else
                model.mode
    in
    { model
        | kmEditorTypeHintInputModal = setSelected kmEditor model.kmEditorTypeHintInputModal
        , mode = newMode
    }



-- MSG


type Msg
    = QuestionnaireTypeHintInputMsg (TypeHintInput.Msg QuestionnaireSuggestion)
    | QuestionnaireTypeHintInputSelect Uuid
    | KnowledgeModelEditorTypeHintInputMsg (TypeHintInput.Msg KnowledgeModelEditorSuggestion)
    | KnowledgeModelEditorTypeHintInputSelect Uuid
    | SetMode PreviewMode
    | FormatSelected String
    | PutPreviewSettingsCompleted (Result ApiError DocumentTemplateDraftPreviewSettings)
    | GetPreviewRequest
    | GetPreviewCompleted (Result ApiError ( Http.Metadata, Maybe UrlResponse ))
    | LoadPreview


loadPreviewMsg : Msg
loadPreviewMsg =
    LoadPreview



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map QuestionnaireTypeHintInputMsg <|
            TypeHintInput.subscriptions model.questionnaireHintInputModel
        , Sub.map KnowledgeModelEditorTypeHintInputMsg <|
            TypeHintInput.subscriptions model.kmEditorTypeHintInputModal
        ]



-- UPDATE


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , documentTemplateId : String
    , documentTemplate : ActionResult DocumentTemplateDraftDetail
    , updatePreviewSettings : DocumentTemplateDraftPreviewSettings -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        getPreviewCmd =
            DocumentTemplateDraftsApi.getPreview appState cfg.documentTemplateId (cfg.wrapMsg << GetPreviewCompleted)

        updatePreviewSettings updateFn =
            let
                previewSettings =
                    ActionResult.unwrap
                        DocumentTemplateDraftPreviewSettings.init
                        DocumentTemplateDraftDetail.getPreviewSettings
                        cfg.documentTemplate
            in
            DocumentTemplateDraftsApi.putPreviewSettings appState
                cfg.documentTemplateId
                (updateFn previewSettings)
                (cfg.wrapMsg << PutPreviewSettingsCompleted)
    in
    case msg of
        QuestionnaireTypeHintInputMsg typeHintInputMsg ->
            let
                updateCfg =
                    { wrapMsg = cfg.wrapMsg << QuestionnaireTypeHintInputMsg
                    , getTypeHints = QuestionnairesApi.getQuestionnaireSuggestions appState PaginationQueryFilters.empty
                    , getError = gettext "Unable to get projects." appState.locale
                    , setReply = cfg.wrapMsg << QuestionnaireTypeHintInputSelect << .uuid
                    , clearReply = Nothing
                    , filterResults = Nothing
                    }

                ( typeHintInputModel, typeHintInputCmd ) =
                    TypeHintInput.update updateCfg typeHintInputMsg model.questionnaireHintInputModel
            in
            ( { model | questionnaireHintInputModel = typeHintInputModel }, typeHintInputCmd )

        QuestionnaireTypeHintInputSelect uuid ->
            ( model, updatePreviewSettings (setQuestionnaireUuid (Just uuid)) )

        KnowledgeModelEditorTypeHintInputMsg typeHintInputMsg ->
            let
                updateCfg =
                    { wrapMsg = cfg.wrapMsg << KnowledgeModelEditorTypeHintInputMsg
                    , getTypeHints = KnowledgeModelEditorsApi.getKnowledgeModelEditorSuggestions appState PaginationQueryFilters.empty
                    , getError = gettext "Unable to get knowledge model editors." appState.locale
                    , setReply = cfg.wrapMsg << KnowledgeModelEditorTypeHintInputSelect << .uuid
                    , clearReply = Nothing
                    , filterResults = Nothing
                    }

                ( typeHintInputModel, typeHintInputCmd ) =
                    TypeHintInput.update updateCfg typeHintInputMsg model.kmEditorTypeHintInputModal
            in
            ( { model | kmEditorTypeHintInputModal = typeHintInputModel }, typeHintInputCmd )

        KnowledgeModelEditorTypeHintInputSelect uuid ->
            ( model, updatePreviewSettings (setKnowledgeModelEditorUuid (Just uuid)) )

        SetMode mode ->
            ( { model
                | mode = mode
                , questionnaireHintInputModel = TypeHintInput.clear model.questionnaireHintInputModel
                , kmEditorTypeHintInputModal = TypeHintInput.clear model.kmEditorTypeHintInputModal
              }
            , updatePreviewSettings DocumentTemplateDraftPreviewSettings.clearQuestionnaireAndKmEditor
            )

        FormatSelected uuidString ->
            ( model, updatePreviewSettings (setFormatUuid (Just (Uuid.fromUuidString uuidString))) )

        PutPreviewSettingsCompleted result ->
            case result of
                Ok previewSettings ->
                    let
                        dispatchUpdateCmd =
                            Task.dispatch (cfg.updatePreviewSettings previewSettings)
                    in
                    if DocumentTemplateDraftPreviewSettings.isPreviewSet previewSettings then
                        ( { model | urlResponse = ActionResult.Loading }
                        , Cmd.batch
                            [ getPreviewCmd
                            , dispatchUpdateCmd
                            ]
                        )

                    else
                        ( model, dispatchUpdateCmd )

                Err _ ->
                    ( model, Cmd.none )

        GetPreviewRequest ->
            ( model, getPreviewCmd )

        GetPreviewCompleted result ->
            case result of
                Ok ( metadata, mbUrlResponse ) ->
                    case ( metadata.statusCode, mbUrlResponse ) of
                        ( 202, _ ) ->
                            ( model, Task.perform (always (cfg.wrapMsg GetPreviewRequest)) (Process.sleep 1000) )

                        ( 200, Just urlResponse ) ->
                            ( { model | urlResponse = ActionResult.Success urlResponse }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    let
                        previewError =
                            ActionResult.Error <|
                                case ApiError.toServerError error of
                                    Just (ServerError.SystemLogError data) ->
                                        String.format data.defaultMessage data.params

                                    _ ->
                                        gettext "Unable to get the document preview." appState.locale
                    in
                    ( { model | urlResponse = previewError }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        LoadPreview ->
            ( { model | urlResponse = ActionResult.Loading }, getPreviewCmd )



-- VIEW


type alias ViewConfig =
    { documentTemplate : DocumentTemplateDraftDetail }


view : ViewConfig -> AppState -> Model -> Html Msg
view cfg appState model =
    let
        previewSettings =
            DocumentTemplateDraftDetail.getPreviewSettings cfg.documentTemplate

        formatOption format =
            option [ value (Uuid.toString format.uuid), selected (Just format.uuid == previewSettings.formatUuid) ]
                [ text format.name ]

        ( typeHintInput, link ) =
            case model.mode of
                QuestionnaireMode ->
                    let
                        projectTypeHintInputCfg =
                            { viewItem = TypeHintInputItem.simple .name
                            , wrapMsg = QuestionnaireTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = False
                            , locale = appState.locale
                            }

                        projectTypeHintInput =
                            TypeHintInput.view projectTypeHintInputCfg model.questionnaireHintInputModel False

                        projectLink =
                            case model.questionnaireHintInputModel.selected of
                                Just questionnaireSuggestion ->
                                    linkTo (Routes.projectsDetail questionnaireSuggestion.uuid)
                                        (class "source-link" :: target "_blank" :: tooltip (gettext "Open project" appState.locale))
                                        [ fa "fa-external-link-alt" ]

                                Nothing ->
                                    Html.nothing
                    in
                    ( projectTypeHintInput, projectLink )

                KMEditorMode ->
                    let
                        kmEditorTypeHintInputCfg =
                            { viewItem = TypeHintInputItem.simple .name
                            , wrapMsg = KnowledgeModelEditorTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = False
                            , locale = appState.locale
                            }

                        kmEditorTypeHintInput =
                            TypeHintInput.view kmEditorTypeHintInputCfg model.kmEditorTypeHintInputModal False

                        kmEdtiorLink =
                            case model.kmEditorTypeHintInputModal.selected of
                                Just kmEditorSuggestion ->
                                    linkTo (Routes.kmEditorEditor kmEditorSuggestion.uuid Nothing)
                                        (class "source-link" :: target "_blank" :: tooltip (gettext "Open KM editor" appState.locale))
                                        [ fa "fa-external-link-alt" ]

                                Nothing ->
                                    Html.nothing
                    in
                    ( kmEditorTypeHintInput, kmEdtiorLink )

        content =
            if DocumentTemplateDraftDetail.isPreviewSet cfg.documentTemplate then
                Page.actionResultViewWithError appState (viewContent appState) viewError model.urlResponse

            else
                viewNotSet appState

        modeSelect =
            div [ class "btn-group" ]
                [ button
                    [ class "btn"
                    , classList
                        [ ( "btn-primary", model.mode == QuestionnaireMode )
                        , ( "btn-outline-primary", model.mode /= QuestionnaireMode )
                        ]
                    , onClick (SetMode QuestionnaireMode)
                    , dataCy "dt-editor_preview-mode_project"
                    ]
                    [ text (gettext "Project" appState.locale) ]
                , button
                    [ class "btn"
                    , classList
                        [ ( "btn-primary", model.mode == KMEditorMode )
                        , ( "btn-outline-primary", model.mode /= KMEditorMode )
                        ]
                    , onClick (SetMode KMEditorMode)
                    , dataCy "dt-editor_preview-mode_km-editor"
                    ]
                    [ text (gettext "KM editor" appState.locale) ]
                ]
    in
    div [ class "DocumentTemplateEditor__PreviewEditor w-100 h-100 d-flex flex-column " ]
        [ div [ class "DocumentTemplateEditor__PreviewEditor__Toolbar bg-light d-flex align-items-center" ]
            [ modeSelect
            , typeHintInput
            , link
            , text (gettext "Format" appState.locale)
            , text ":"
            , select [ class "form-select", onInput FormatSelected, id "format", name "format" ]
                (option [ value "" ] [ text "--" ] :: List.map formatOption cfg.documentTemplate.formats)
            ]
        , div [ class "flex-grow-1" ]
            [ content ]
        ]


viewNotSet : AppState -> Html msg
viewNotSet appState =
    Page.illustratedMessage
        { illustration = Undraw.settingsTab
        , heading = gettext "Preview not set" appState.locale
        , lines = [ gettext "Select a project or knowledge model editor and format you want to preview." appState.locale ]
        , cy = "preview-not-set"
        }


viewContent : AppState -> UrlResponse -> Html msg
viewContent appState urlResponse =
    if ContentType.isSupportedInBrowser appState.navigator urlResponse.contentType then
        iframe [ src urlResponse.url, class "w-100 h-100", dataCy "document-preview" ] []

    else
        viewNotSupported appState urlResponse.url


viewError : String -> Html msg
viewError error =
    pre [ class "pre-error m-3" ] [ text error ]


viewNotSupported : AppState -> String -> Html msg
viewNotSupported appState documentUrl =
    Page.illustratedMessageHtml
        { illustration = Undraw.downloadFiles
        , heading = gettext "Download preview" appState.locale
        , content =
            [ p [] [ text (gettext "The document format cannot be displayed in the web browser. You can still download and view it." appState.locale) ]
            , p []
                [ a [ class "btn btn-primary btn-lg with-icon", href documentUrl, target "_blank" ]
                    [ faDownload
                    , text (gettext "Download" appState.locale)
                    ]
                ]
            ]
        , cy = "format-not-supported"
        }
