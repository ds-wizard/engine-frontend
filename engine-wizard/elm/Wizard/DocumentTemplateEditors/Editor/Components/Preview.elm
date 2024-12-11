module Wizard.DocumentTemplateEditors.Editor.Components.Preview exposing
    ( Model
    , Msg
    , PreviewMode
    , UpdateConfig
    , ViewConfig
    , initialModel
    , loadPreviewMsg
    , setSelectedBranch
    , setSelectedQuestionnaire
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, iframe, option, p, pre, select, text)
import Html.Attributes exposing (class, classList, href, id, name, selected, src, target, value)
import Html.Events exposing (onClick, onInput)
import Http
import Maybe.Extra as Maybe
import Process
import Shared.Api.Branches as BranchesApi
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.BranchSuggestion exposing (BranchSuggestion)
import Shared.Data.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Shared.Data.DocumentTemplateDraftDetail as DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.QuestionnaireSuggestion exposing (QuestionnaireSuggestion)
import Shared.Data.UrlResponse exposing (UrlResponse)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Error.ServerError as ServerError
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Setters exposing (setBranchUuid, setFormatUuid, setQuestionnaireUuid, setSelected)
import Shared.Undraw as Undraw
import Shared.Utils exposing (dispatch)
import String.Format as String
import Task
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.ContentType as ContentType
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltip)
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes



-- MODEL


type alias Model =
    { questionnaireHintInputModel : TypeHintInput.Model QuestionnaireSuggestion
    , branchTypeHintInputModal : TypeHintInput.Model BranchSuggestion
    , urlResponse : ActionResult UrlResponse
    , mode : PreviewMode
    }


type PreviewMode
    = QuestionnaireMode
    | BranchMode


initialModel : Model
initialModel =
    { questionnaireHintInputModel = TypeHintInput.init "uuid"
    , branchTypeHintInputModal = TypeHintInput.init "uuid"
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


setSelectedBranch : Maybe BranchSuggestion -> Model -> Model
setSelectedBranch branch model =
    let
        newMode =
            if Maybe.isJust branch then
                BranchMode

            else
                model.mode
    in
    { model
        | branchTypeHintInputModal = setSelected branch model.branchTypeHintInputModal
        , mode = newMode
    }



-- MSG


type Msg
    = QuestionnaireTypeHintInputMsg (TypeHintInput.Msg QuestionnaireSuggestion)
    | QuestionnaireTypeHintInputSelect Uuid
    | BranchTypeHintInputMsg (TypeHintInput.Msg BranchSuggestion)
    | BranchTypeHintInputSelect Uuid
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
        , Sub.map BranchTypeHintInputMsg <|
            TypeHintInput.subscriptions model.branchTypeHintInputModal
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
            DocumentTemplateDraftsApi.getPreview cfg.documentTemplateId appState (cfg.wrapMsg << GetPreviewCompleted)

        updatePreviewSettings updateFn =
            let
                previewSettings =
                    ActionResult.unwrap
                        DocumentTemplateDraftPreviewSettings.init
                        DocumentTemplateDraftDetail.getPreviewSettings
                        cfg.documentTemplate
            in
            DocumentTemplateDraftsApi.putPreviewSettings
                cfg.documentTemplateId
                (updateFn previewSettings)
                appState
                (cfg.wrapMsg << PutPreviewSettingsCompleted)
    in
    case msg of
        QuestionnaireTypeHintInputMsg typeHintInputMsg ->
            let
                updateCfg =
                    { wrapMsg = cfg.wrapMsg << QuestionnaireTypeHintInputMsg
                    , getTypeHints = QuestionnairesApi.getQuestionnaireSuggestions PaginationQueryFilters.empty
                    , getError = gettext "Unable to get projects." appState.locale
                    , setReply = cfg.wrapMsg << QuestionnaireTypeHintInputSelect << .uuid
                    , clearReply = Nothing
                    , filterResults = Nothing
                    }

                ( typeHintInputModel, typeHintInputCmd ) =
                    TypeHintInput.update updateCfg typeHintInputMsg appState model.questionnaireHintInputModel
            in
            ( { model | questionnaireHintInputModel = typeHintInputModel }, typeHintInputCmd )

        QuestionnaireTypeHintInputSelect uuid ->
            ( model, updatePreviewSettings (setQuestionnaireUuid (Just uuid)) )

        BranchTypeHintInputMsg typeHintInputMsg ->
            let
                updateCfg =
                    { wrapMsg = cfg.wrapMsg << BranchTypeHintInputMsg
                    , getTypeHints = BranchesApi.getBranchSuggestions PaginationQueryFilters.empty
                    , getError = gettext "Unable to get KM editors." appState.locale
                    , setReply = cfg.wrapMsg << BranchTypeHintInputSelect << .uuid
                    , clearReply = Nothing
                    , filterResults = Nothing
                    }

                ( typeHintInputModel, typeHintInputCmd ) =
                    TypeHintInput.update updateCfg typeHintInputMsg appState model.branchTypeHintInputModal
            in
            ( { model | branchTypeHintInputModal = typeHintInputModel }, typeHintInputCmd )

        BranchTypeHintInputSelect uuid ->
            ( model, updatePreviewSettings (setBranchUuid (Just uuid)) )

        SetMode mode ->
            ( { model
                | mode = mode
                , questionnaireHintInputModel = TypeHintInput.clear model.questionnaireHintInputModel
                , branchTypeHintInputModal = TypeHintInput.clear model.branchTypeHintInputModal
              }
            , updatePreviewSettings DocumentTemplateDraftPreviewSettings.clearQuestionnaireAndBranch
            )

        FormatSelected uuidString ->
            ( model, updatePreviewSettings (setFormatUuid (Just (Uuid.fromUuidString uuidString))) )

        PutPreviewSettingsCompleted result ->
            case result of
                Ok previewSettings ->
                    let
                        dispatchUpdateCmd =
                            dispatch (cfg.updatePreviewSettings previewSettings)
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
                    , getResultCmd cfg.logoutMsg result
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
                            { viewItem = TypeHintItem.simple .name
                            , wrapMsg = QuestionnaireTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = False
                            }

                        projectTypeHintInput =
                            TypeHintInput.view appState projectTypeHintInputCfg model.questionnaireHintInputModel False

                        projectLink =
                            case model.questionnaireHintInputModel.selected of
                                Just questionnaireSuggestion ->
                                    linkTo appState
                                        (Routes.projectsDetail questionnaireSuggestion.uuid)
                                        (class "source-link" :: target "_blank" :: tooltip (gettext "Open project" appState.locale))
                                        [ fa "fa-external-link-alt" ]

                                Nothing ->
                                    emptyNode
                    in
                    ( projectTypeHintInput, projectLink )

                BranchMode ->
                    let
                        branchTypeHintInputCfg =
                            { viewItem = TypeHintItem.simple .name
                            , wrapMsg = BranchTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = False
                            }

                        branchTypeHintInput =
                            TypeHintInput.view appState branchTypeHintInputCfg model.branchTypeHintInputModal False

                        branchLink =
                            case model.branchTypeHintInputModal.selected of
                                Just branchSuggestion ->
                                    linkTo appState
                                        (Routes.kmEditorEditor branchSuggestion.uuid Nothing)
                                        (class "source-link" :: target "_blank" :: tooltip (gettext "Open KM editor" appState.locale))
                                        [ fa "fa-external-link-alt" ]

                                Nothing ->
                                    emptyNode
                    in
                    ( branchTypeHintInput, branchLink )

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
                    ]
                    [ text (gettext "Project" appState.locale) ]
                , button
                    [ class "btn"
                    , classList
                        [ ( "btn-primary", model.mode == BranchMode )
                        , ( "btn-outline-primary", model.mode /= BranchMode )
                        ]
                    , onClick (SetMode BranchMode)
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
        { image = Undraw.settingsTab
        , heading = gettext "Preview not set" appState.locale
        , lines = [ gettext "Select project or KM editor and format you want to preview." appState.locale ]
        , cy = "preview-not-set"
        }


viewContent : AppState -> UrlResponse -> Html msg
viewContent appState urlResponse =
    if ContentType.isSupportedInBrowser appState urlResponse.contentType then
        iframe [ src urlResponse.url, class "w-100 h-100", dataCy "document-preview" ] []

    else
        viewNotSupported appState urlResponse.url


viewError : String -> Html msg
viewError error =
    pre [ class "pre-error m-3" ] [ text error ]


viewNotSupported : AppState -> String -> Html msg
viewNotSupported appState documentUrl =
    Page.illustratedMessageHtml
        { image = Undraw.downloadFiles
        , heading = gettext "Download preview" appState.locale
        , content =
            [ p [] [ text (gettext "The document format cannot be displayed in the web browser. You can still download and view it." appState.locale) ]
            , p []
                [ a [ class "btn btn-primary btn-lg with-icon", href documentUrl, target "_blank" ]
                    [ faSet "_global.download" appState
                    , text (gettext "Download" appState.locale)
                    ]
                ]
            ]
        , cy = "format-not-supported"
        }
