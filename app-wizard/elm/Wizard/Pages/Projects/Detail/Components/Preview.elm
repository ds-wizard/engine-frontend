module Wizard.Pages.Projects.Detail.Components.Preview exposing
    ( Model
    , Msg
    , PreviewState(..)
    , fetchData
    , init
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Events
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Api.ServerError as ServerError
import Common.Components.FontAwesome exposing (faArrowRight, faDownload)
import Common.Components.Page as Page
import Common.Components.Undraw as Undraw
import Common.Utils.ContentType as ContentType
import Gettext exposing (gettext)
import Html exposing (Html, a, div, iframe, p, pre, text)
import Html.Attributes exposing (class, href, src, target)
import Html.Attributes.Extensions exposing (dataCy)
import Http
import Process
import String.Format as String
import Task
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectPreview exposing (ProjectPreview)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session
import Wizard.Routes as Routes
import Wizard.Utils.ProjectUtils as ProjectUtils



-- MODEL


type alias Model =
    { projectUuid : Uuid
    , previewState : PreviewState
    , visibility : Browser.Events.Visibility
    , reloadWhenVisible : Bool
    , waitingForFrame : Bool
    }


type PreviewState
    = TemplateNotSet
    | TemplateUnsupported
    | Preview (ActionResult UrlResponse)


init : Uuid -> PreviewState -> Model
init uuid previewState =
    { projectUuid = uuid
    , previewState = previewState
    , visibility = Browser.Events.Visible
    , reloadWhenVisible = False
    , waitingForFrame = False
    }


isHidden : Model -> Bool
isHidden model =
    model.visibility == Browser.Events.Hidden



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onVisibilityChange VisibilityChanged
        , if model.waitingForFrame then
            Browser.Events.onAnimationFrame (always AnimationFrame)

          else
            Sub.none
        ]



-- UPDATE


type Msg
    = GetDocumentPreviewComplete (Result ApiError ( Http.Metadata, Maybe UrlResponse ))
    | ScheduleRequest
    | AnimationFrame
    | VisibilityChanged Browser.Events.Visibility


fetchData : Bool -> Cmd Msg
fetchData hasTemplate =
    if hasTemplate then
        Task.dispatch ScheduleRequest

    else
        Cmd.none


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        GetDocumentPreviewComplete result ->
            handleHeadDocumentPreviewComplete appState model result

        -- Requests are never fired directly. Elm renders on animation frames, which are paused
        -- while the page is not rendered (hidden tab, minimized window). Waiting for a frame
        -- before asking for a URL guarantees that the page can actually paint the iframe with
        -- it, instead of parking a short lived signed URL in the model until the user comes
        -- back to the tab minutes later.
        ScheduleRequest ->
            ( { model | waitingForFrame = True }, Cmd.none )

        AnimationFrame ->
            if model.waitingForFrame then
                ( { model | waitingForFrame = False }
                , ProjectsApi.getDocumentPreview appState model.projectUuid GetDocumentPreviewComplete
                )

            else
                ( model, Cmd.none )

        VisibilityChanged visibility ->
            let
                newModel =
                    { model | visibility = visibility }
            in
            if visibility == Browser.Events.Visible && model.reloadWhenVisible then
                ( { newModel | reloadWhenVisible = False, waitingForFrame = True }, Cmd.none )

            else
                ( newModel, Cmd.none )


handleHeadDocumentPreviewComplete : AppState -> Model -> Result ApiError ( Http.Metadata, Maybe UrlResponse ) -> ( Model, Cmd Msg )
handleHeadDocumentPreviewComplete appState model result =
    case result of
        Ok ( metadata, mbUrlResponse ) ->
            case ( metadata.statusCode, mbUrlResponse ) of
                ( 202, _ ) ->
                    ( model, Task.perform (always ScheduleRequest) (Process.sleep 1000) )

                ( 200, Just urlResponse ) ->
                    -- The page may still have been hidden between issuing the request and getting
                    -- the response. The signed URL expires within a minute, so drop it and ask for
                    -- a fresh one once the page is visible again.
                    if isHidden model then
                        ( { model | reloadWhenVisible = True }, Cmd.none )

                    else
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


view : AppState -> ProjectPreview -> Model -> Html Msg
view appState project model =
    case model.previewState of
        Preview preview ->
            Page.actionResultViewWithError appState (viewContent appState) viewError preview

        TemplateNotSet ->
            viewTemplateNotSet appState project

        TemplateUnsupported ->
            viewTemplateUnsupported appState project


viewContent : AppState -> UrlResponse -> Html Msg
viewContent appState urlResponse =
    if ContentType.isSupportedInBrowser appState.navigator urlResponse.contentType then
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


viewTemplateNotSet : AppState -> ProjectPreview -> Html msg
viewTemplateNotSet appState project =
    let
        content =
            if not (Session.exists appState.session) then
                [ p [] [ text (gettext "Log in to set a default document template and format." appState.locale) ]
                ]

            else if ProjectUtils.isOwner appState project then
                [ p [] [ text (gettext "Before you can use preview you need to set a default document template and format." appState.locale) ]
                , p []
                    [ linkTo (Routes.projectsDetailSettings project.uuid)
                        [ class "btn btn-primary btn-lg with-icon-after" ]
                        [ text (gettext "Go to settings" appState.locale)
                        , faArrowRight
                        ]
                    ]
                ]

            else
                [ p [] [ text (gettext "Ask the Project owner to set a default document template and format." appState.locale) ]
                ]
    in
    Page.illustratedMessageHtml
        { illustration = Undraw.websiteBuilder
        , heading = gettext "Default document template is not set." appState.locale
        , content = content
        , cy = "template-not-set"
        }


viewTemplateUnsupported : AppState -> ProjectPreview -> Html msg
viewTemplateUnsupported appState project =
    let
        content =
            if not (Session.exists appState.session) then
                [ p [] [ text (gettext "Log in to update the default document template." appState.locale) ]
                ]

            else if ProjectUtils.isOwner appState project then
                [ p [] [ text (gettext "Before you can use preview you need to update the default document template." appState.locale) ]
                , p []
                    [ linkTo (Routes.projectsDetailSettings project.uuid)
                        [ class "btn btn-primary btn-lg with-icon-after" ]
                        [ text (gettext "Go to settings" appState.locale)
                        , faArrowRight
                        ]
                    ]
                ]

            else
                [ p [] [ text (gettext "Ask the project owner to update the default document template." appState.locale) ]
                ]
    in
    Page.illustratedMessageHtml
        { illustration = Undraw.warning
        , heading = gettext "Default document template is no longer supported." appState.locale
        , content = content
        , cy = "template-not-set"
        }
