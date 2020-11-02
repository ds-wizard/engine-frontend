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
import Dict
import Html exposing (Html, a, div, iframe, p)
import Html.Attributes exposing (class, href, src, target)
import Http
import Maybe.Extra as Maybe
import Process
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lg, lx)
import Task
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes as ProjectRoutes
import Wizard.Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.Preview"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.Components.Preview"



-- MODEL


type alias Model =
    { questionnaireUuid : Uuid
    , previewState : PreviewState
    }


type PreviewState
    = TemplateNotSet
    | Preview (ActionResult (Maybe String))


init : Uuid -> PreviewState -> Model
init uuid previewState =
    { questionnaireUuid = uuid
    , previewState = previewState
    }



-- UPDATE


type Msg
    = HeadDocumentPreviewComplete (Result ApiError Http.Metadata)
    | HeadRequest


fetchData : AppState -> Uuid -> Bool -> Cmd Msg
fetchData appState questionnaireUuid hasTemplate =
    if hasTemplate then
        QuestionnairesApi.headDocumentPreview questionnaireUuid appState HeadDocumentPreviewComplete

    else
        Cmd.none


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        HeadDocumentPreviewComplete result ->
            handleHeadDocumentPreviewComplete appState model result

        HeadRequest ->
            ( model, fetchData appState model.questionnaireUuid True )


handleHeadDocumentPreviewComplete : AppState -> Model -> Result ApiError Http.Metadata -> ( Model, Cmd Msg )
handleHeadDocumentPreviewComplete appState model result =
    case result of
        Ok metadata ->
            if metadata.statusCode == 202 then
                ( model
                , Process.sleep 1000
                    |> Task.perform (always HeadRequest)
                )

            else
                ( { model | previewState = Preview (Success (Dict.get "content-type" metadata.headers)) }, Cmd.none )

        Err _ ->
            ( { model | previewState = Preview (Error (lg "apiError.questionnaires.headDocumentPreview" appState)) }, Cmd.none )



-- VIEW


view : AppState -> QuestionnaireDetail -> Model -> Html Msg
view appState questionnaire model =
    let
        content =
            case model.previewState of
                Preview preview ->
                    Page.actionResultView appState (viewContent appState model) preview

                TemplateNotSet ->
                    viewTemplateNotSet appState questionnaire
    in
    content


viewContent : AppState -> Model -> Maybe String -> Html Msg
viewContent appState model mbContentType =
    let
        documentUrl =
            QuestionnairesApi.documentPreviewUrl model.questionnaireUuid appState
    in
    if Maybe.unwrap False (isSupportedInBrowser appState) mbContentType then
        div [ class "Plans__Detail__Content Plans__Detail__Content--Preview" ]
            [ iframe [ src documentUrl ] [] ]

    else
        viewNotSupported appState documentUrl


viewNotSupported : AppState -> String -> Html msg
viewNotSupported appState documentUrl =
    Page.illustratedMessageHtml
        { image = "download_files"
        , heading = l_ "notSupported.title" appState
        , content =
            [ p [] [ lx_ "notSupported.text" appState ]
            , p []
                [ a [ class "btn btn-primary btn-lg link-with-icon", href documentUrl, target "_blank" ]
                    [ faSet "_global.download" appState
                    , lx_ "notSupported.download" appState
                    ]
                ]
            ]
        }


viewTemplateNotSet : AppState -> QuestionnaireDetail -> Html msg
viewTemplateNotSet appState questionnaire =
    let
        content =
            if QuestionnaireDetail.isOwner appState questionnaire then
                [ p [] [ lx_ "templateNotSet.textOwner" appState ]
                , p []
                    [ linkTo appState
                        (Wizard.Routes.ProjectsRoute (ProjectRoutes.DetailRoute questionnaire.uuid PlanDetailRoute.Settings))
                        [ class "btn btn-primary btn-lg link-with-icon-after" ]
                        [ lx_ "templateNotSet.link" appState
                        , faSet "_global.arrowRight" appState
                        ]
                    ]
                ]

            else
                [ p [] [ lx_ "templateNotSet.textNotOwner" appState ]
                ]
    in
    Page.illustratedMessageHtml
        { image = "website_builder"
        , heading = l_ "templateNotSet.heading" appState
        , content = content
        }


isSupportedInBrowser : AppState -> String -> Bool
isSupportedInBrowser appState contentType =
    if contentType == "application/pdf" then
        appState.navigator.pdf

    else
        String.startsWith "text/" contentType || List.member contentType supportedMimeTypes


supportedMimeTypes : List String
supportedMimeTypes =
    [ "application/json", "application/ld+json" ]
