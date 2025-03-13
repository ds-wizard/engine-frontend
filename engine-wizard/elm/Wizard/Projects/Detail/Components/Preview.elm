module Wizard.Projects.Detail.Components.Preview exposing
    ( Model
    , Msg
    , PreviewState(..)
    , fetchData
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, a, div, iframe, p, pre, text)
import Html.Attributes exposing (class, href, src, target)
import Http
import Process
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Auth.Session as Session
import Shared.Data.QuestionnairePreview exposing (QuestionnairePreview)
import Shared.Data.UrlResponse exposing (UrlResponse)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Error.ServerError as ServerError
import Shared.Html exposing (faSet)
import Shared.Undraw as Undraw
import String.Format as String
import Task
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.ContentType as ContentType
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.QuestionnaireUtils as QuestionnaireUtils
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes



-- MODEL


type alias Model =
    { questionnaireUuid : Uuid
    , previewState : PreviewState
    }


type PreviewState
    = TemplateNotSet
    | TemplateUnsupported
    | Preview (ActionResult UrlResponse)


init : Uuid -> PreviewState -> Model
init uuid previewState =
    { questionnaireUuid = uuid
    , previewState = previewState
    }



-- UPDATE


type Msg
    = GetDocumentPreviewComplete (Result ApiError ( Http.Metadata, Maybe UrlResponse ))
    | HeadRequest


fetchData : AppState -> Uuid -> Bool -> Cmd Msg
fetchData appState questionnaireUuid hasTemplate =
    if hasTemplate then
        QuestionnairesApi.getDocumentPreview questionnaireUuid appState GetDocumentPreviewComplete

    else
        Cmd.none


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        GetDocumentPreviewComplete result ->
            handleHeadDocumentPreviewComplete appState model result

        HeadRequest ->
            ( model, fetchData appState model.questionnaireUuid True )


handleHeadDocumentPreviewComplete : AppState -> Model -> Result ApiError ( Http.Metadata, Maybe UrlResponse ) -> ( Model, Cmd Msg )
handleHeadDocumentPreviewComplete appState model result =
    case result of
        Ok ( metadata, mbUrlResponse ) ->
            case ( metadata.statusCode, mbUrlResponse ) of
                ( 202, _ ) ->
                    ( model, Task.perform (always HeadRequest) (Process.sleep 1000) )

                ( 200, Just urlResponse ) ->
                    ( { model | previewState = Preview (Success urlResponse) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Err apiError ->
            let
                previewError =
                    Preview (Error (gettext "Unable to get the document preview." appState.locale))

                previewState =
                    case ApiError.toServerError apiError of
                        Just (ServerError.SystemLogError data) ->
                            Preview (Error (String.format data.defaultMessage data.params))

                        Just (ServerError.UserSimpleError message) ->
                            if message.code == "error.validation.tml_unsupported_metamodel_version" then
                                TemplateUnsupported

                            else
                                previewError

                        _ ->
                            previewError
            in
            ( { model | previewState = previewState }, Cmd.none )



-- VIEW


view : AppState -> QuestionnairePreview -> Model -> Html Msg
view appState questionnaire model =
    case model.previewState of
        Preview preview ->
            Page.actionResultViewWithError appState (viewContent appState) viewError preview

        TemplateNotSet ->
            viewTemplateNotSet appState questionnaire

        TemplateUnsupported ->
            viewTemplateUnsupported appState questionnaire


viewContent : AppState -> UrlResponse -> Html Msg
viewContent appState urlResponse =
    if ContentType.isSupportedInBrowser appState urlResponse.contentType then
        div [ class "Projects__Detail__Content Projects__Detail__Content--Preview" ]
            [ iframe [ src urlResponse.url ] [] ]

    else
        viewNotSupported appState urlResponse.url


viewError : String -> Html Msg
viewError msg =
    div [ class "Projects__Detail__Content m-3", dataCy "project_preview_error" ]
        [ pre [ class "pre-error" ] [ text msg ]
        ]


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


viewTemplateNotSet : AppState -> QuestionnairePreview -> Html msg
viewTemplateNotSet appState questionnaire =
    let
        content =
            if not (Session.exists appState.session) then
                [ p [] [ text (gettext "Log in to set a default document template and format." appState.locale) ]
                ]

            else if QuestionnaireUtils.isOwner appState questionnaire then
                [ p [] [ text (gettext "Before you can use preview you need to set a default document template and format." appState.locale) ]
                , p []
                    [ linkTo appState
                        (Routes.projectsDetailSettings questionnaire.uuid)
                        [ class "btn btn-primary btn-lg with-icon-after" ]
                        [ text (gettext "Go to settings" appState.locale)
                        , faSet "_global.arrowRight" appState
                        ]
                    ]
                ]

            else
                [ p [] [ text (gettext "Ask the Project owner to set a default document template and format." appState.locale) ]
                ]
    in
    Page.illustratedMessageHtml
        { image = Undraw.websiteBuilder
        , heading = gettext "Default document template is not set." appState.locale
        , content = content
        , cy = "template-not-set"
        }


viewTemplateUnsupported : AppState -> QuestionnairePreview -> Html msg
viewTemplateUnsupported appState questionnaire =
    let
        content =
            if not (Session.exists appState.session) then
                [ p [] [ text (gettext "Log in to update the default document template." appState.locale) ]
                ]

            else if QuestionnaireUtils.isOwner appState questionnaire then
                [ p [] [ text (gettext "Before you can use preview you need to update the default document template." appState.locale) ]
                , p []
                    [ linkTo appState
                        (Routes.projectsDetailSettings questionnaire.uuid)
                        [ class "btn btn-primary btn-lg with-icon-after" ]
                        [ text (gettext "Go to settings" appState.locale)
                        , faSet "_global.arrowRight" appState
                        ]
                    ]
                ]

            else
                [ p [] [ text (gettext "Ask the project owner to update the default document template." appState.locale) ]
                ]
    in
    Page.illustratedMessageHtml
        { image = Undraw.warning
        , heading = gettext "Default document template is no longer supported." appState.locale
        , content = content
        , cy = "template-not-set"
        }
