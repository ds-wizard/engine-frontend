module Wizard.DocumentTemplateEditors.Editor.Components.Preview exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , initialModel
    , loadPreviewMsg
    , setSelectedQuestionnaire
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, a, div, iframe, option, p, pre, select, text)
import Html.Attributes exposing (class, href, id, name, selected, src, target, value)
import Html.Events exposing (onInput)
import Http
import Maybe.Extra as Maybe
import Process
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Shared.Data.DocumentTemplateDraftDetail as DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Data.QuestionnaireSuggestion exposing (QuestionnaireSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Error.ServerError as ServerError
import Shared.Html exposing (faSet)
import Shared.Setters exposing (setFormatUuid, setQuestionnaireUuid, setSelected)
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
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Page as Page



-- MODEL


type alias Model =
    { typeHintInputModel : TypeHintInput.Model QuestionnaireSuggestion
    , contentType : ActionResult (Maybe String)
    }


initialModel : Model
initialModel =
    { typeHintInputModel = TypeHintInput.init "uuid"
    , contentType = ActionResult.Unset
    }


setSelectedQuestionnaire : Maybe QuestionnaireSuggestion -> Model -> Model
setSelectedQuestionnaire questionnaire model =
    { model | typeHintInputModel = setSelected questionnaire model.typeHintInputModel }



-- MSG


type Msg
    = QuestionnaireTypeHintInputMsg (TypeHintInput.Msg QuestionnaireSuggestion)
    | QuestionnaireTypeHintInputSelect Uuid
    | FormatSelected String
    | PutPreviewSettingsCompleted (Result ApiError DocumentTemplateDraftPreviewSettings)
    | GetPreviewCompleted (Result ApiError Http.Metadata)
    | HeadPreviewRequest
    | LoadPreview


loadPreviewMsg : Msg
loadPreviewMsg =
    LoadPreview



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map QuestionnaireTypeHintInputMsg <|
        TypeHintInput.subscriptions model.typeHintInputModel



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
            DocumentTemplateDraftsApi.headPreview cfg.documentTemplateId appState (cfg.wrapMsg << GetPreviewCompleted)

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
                    , getTypeHints =
                        QuestionnairesApi.getQuestionnaireSuggestions
                            { isTemplate = Just False
                            , isMigrating = Just False
                            , userUuids = Nothing
                            , userUuidsOp = Nothing
                            , projectTags = Nothing
                            , projectTagsOp = Nothing
                            , packageIds = Nothing
                            , packageIdsOp = Nothing
                            }
                    , getError = gettext "Unable to get projects." appState.locale
                    , setReply = cfg.wrapMsg << QuestionnaireTypeHintInputSelect << .uuid
                    , clearReply = Nothing
                    , filterResults = Nothing
                    }

                ( typeHintInputModel, typeHintInputCmd ) =
                    TypeHintInput.update updateCfg typeHintInputMsg appState model.typeHintInputModel
            in
            ( { model | typeHintInputModel = typeHintInputModel }, typeHintInputCmd )

        QuestionnaireTypeHintInputSelect uuid ->
            ( model, updatePreviewSettings (setQuestionnaireUuid (Just uuid)) )

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
                        ( { model | contentType = ActionResult.Loading }
                        , Cmd.batch
                            [ getPreviewCmd
                            , dispatchUpdateCmd
                            ]
                        )

                    else
                        ( model, dispatchUpdateCmd )

                Err _ ->
                    ( model, Cmd.none )

        HeadPreviewRequest ->
            ( model, getPreviewCmd )

        GetPreviewCompleted result ->
            case result of
                Ok metadata ->
                    if metadata.statusCode == 202 then
                        ( model, Task.perform (always (cfg.wrapMsg HeadPreviewRequest)) (Process.sleep 1000) )

                    else
                        ( { model | contentType = ActionResult.Success (Dict.get "content-type" metadata.headers) }, Cmd.none )

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
                    ( { model | contentType = previewError }
                    , getResultCmd cfg.logoutMsg result
                    )

        LoadPreview ->
            if ActionResult.unwrap False DocumentTemplateDraftDetail.isPreviewSet cfg.documentTemplate then
                ( { model | contentType = ActionResult.Loading }, getPreviewCmd )

            else
                ( model, Cmd.none )



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

        typeHintInputCfg =
            { viewItem = TypeHintItem.simple .name
            , wrapMsg = QuestionnaireTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            }

        content =
            if DocumentTemplateDraftDetail.isPreviewSet cfg.documentTemplate then
                Page.actionResultViewWithError appState (viewContent cfg appState) viewError model.contentType

            else
                viewNotSet appState
    in
    div [ class "DocumentTemplateEditor__PreviewEditor w-100 h-100 d-flex flex-column " ]
        [ div [ class "DocumentTemplateEditor__PreviewEditor__Toolbar bg-light d-flex align-items-center" ]
            [ text "Project:"
            , TypeHintInput.view appState typeHintInputCfg model.typeHintInputModel False
            , text "Format:"
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
        , lines = [ gettext "Select project and format you want to preview." appState.locale ]
        , cy = "preview-not-set"
        }


viewContent : ViewConfig -> AppState -> Maybe String -> Html msg
viewContent cfg appState mbContentType =
    let
        documentUrl =
            DocumentTemplateDraftsApi.previewUrl cfg.documentTemplate.id appState
    in
    if Maybe.unwrap False (ContentType.isSupportedInBrowser appState) mbContentType then
        iframe [ src documentUrl, class "w-100 h-100", dataCy "document-preview" ] []

    else
        viewNotSupported appState documentUrl


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
