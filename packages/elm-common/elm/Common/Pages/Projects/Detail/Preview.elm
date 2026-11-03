module Common.Pages.Projects.Detail.Preview exposing
    ( Model
    , Msg
    , PreviewState
    , initialModel
    , isDocumentShown
    , setProject
    , subscriptions
    , update
    , view
    )

{-| The preview tab of one project: the document its default template would
produce, shown as it is.

The server renders the document when it is asked for, so the first answer is
usually "not yet" (202) and the tab asks again a second later, until it is ready.

**A request is never fired straight away.** What comes back is a URL that is
signed for about a minute, and Elm paints on animation frames, which stop while
the tab is in the background or the window is minimized. Asking for a URL that
nobody can paint parks a dead link in the model until the user comes back, so
the request waits for a frame first — and a URL that arrives after the page has
been hidden is dropped and asked for again once it is visible.

**A document the browser cannot show is said in words.** Not every format can be
put in a frame; the ones that cannot get a page that says so and a link to
download the document, rather than a picture of a file and nothing else. The
frame itself is named, because an `iframe` with no title is announced as
"frame" and nothing more.

**The tab has no heading, and shows one thing at a time.** The frame above it
has the page's `h1` and the tabs say where the user is, so the tab shows the
full-page loader, a full-page message — its title an `h2`
(`Ui.Page.Message.withinPage`), an error's sentence announced, and a failed
template's own words as code under it (`Common.Components.DocumentPreviewError`)
— or the document.
The document fills the tab edge to edge (`isDocumentShown` is what the frame
asks before it turns its body flush) and scrolls in its own frame.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Events
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.ProjectPreview as ProjectPreview exposing (ProjectPreview)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Api.Projects as ProjectsApi
import Common.Api.ServerError as ServerError
import Common.Components.DocumentPreviewError as DocumentPreviewError exposing (DocumentPreviewError)
import Common.Components.Page as Page
import Common.Data.AppState exposing (AppState)
import Common.Data.ProjectUtils as ProjectUtils
import Common.Data.Session as Session
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Routes as Routes
import Common.Utils.ContentType as ContentType
import Gettext exposing (gettext)
import Html exposing (Html, iframe)
import Html.Attributes exposing (class, src, title)
import Http
import Process
import Task
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Page.Message as Message exposing (Message)
import Ui.TestId as TestId
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { previewState : PreviewState
    , visibility : Browser.Events.Visibility
    , reloadWhenVisible : Bool
    , waitingForFrame : Bool
    }


type PreviewState
    = TemplateNotSet
    | TemplateUnsupported
    | Preview (ActionResult UrlResponse)
    | PreviewFailed DocumentPreviewError


initialModel : Model
initialModel =
    { previewState = Preview Loading
    , visibility = Browser.Events.Visible
    , reloadWhenVisible = False
    , waitingForFrame = False
    }


{-| The project's preview settings arrived. A project with a default template
and format is one there is a document to ask for, and asking begins with waiting
for a frame; a project without one has nothing to preview and says so.
-}
setProject : ProjectPreview -> Model -> Model
setProject project model =
    if ProjectPreview.hasTemplateSet project then
        { model | previewState = Preview Loading, waitingForFrame = True }

    else
        { model | previewState = TemplateNotSet }


isHidden : Model -> Bool
isHidden model =
    model.visibility == Browser.Events.Hidden



-- UPDATE


type Msg
    = GetDocumentPreviewCompleted (Result ApiError ( Http.Metadata, Maybe UrlResponse ))
    | ScheduleRequest
    | AnimationFrame
    | VisibilityChanged Browser.Events.Visibility


update : UpdateConfig Msg msg -> AppState -> Uuid -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState projectUuid msg model =
    case msg of
        GetDocumentPreviewCompleted result ->
            handleDocumentPreviewCompleted cfg appState model result

        ScheduleRequest ->
            ( { model | waitingForFrame = True }, Cmd.none )

        AnimationFrame ->
            if model.waitingForFrame then
                ( { model | waitingForFrame = False }
                , ProjectsApi.getDocumentPreview appState projectUuid (cfg.wrapMsg << GetDocumentPreviewCompleted)
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


handleDocumentPreviewCompleted : UpdateConfig Msg msg -> AppState -> Model -> Result ApiError ( Http.Metadata, Maybe UrlResponse ) -> ( Model, Cmd msg )
handleDocumentPreviewCompleted cfg appState model result =
    case result of
        Ok ( metadata, mbUrlResponse ) ->
            case ( metadata.statusCode, mbUrlResponse ) of
                ( 202, _ ) ->
                    -- Still being rendered. Ask again in a second.
                    ( model, Task.perform (always (cfg.wrapMsg ScheduleRequest)) (Process.sleep 1000) )

                ( 200, Just urlResponse ) ->
                    -- The page may have been hidden between the request and the
                    -- response, and the URL expires within a minute. Drop it and
                    -- ask for a fresh one once the page is visible again.
                    if isHidden model then
                        ( { model | reloadWhenVisible = True }, Cmd.none )

                    else
                        ( { model | previewState = Preview (Success urlResponse) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Err apiError ->
            let
                previewError =
                    PreviewFailed (DocumentPreviewError.fromApiError appState (gettext "Unable to get the document preview." appState.locale) apiError)

                previewState =
                    case ApiError.toServerError apiError of
                        Just (ServerError.UserSimpleError message) ->
                            if message.code == "error.validation.tml_unsupported_metamodel_version" then
                                TemplateUnsupported

                            else
                                previewError

                        _ ->
                            previewError
            in
            ( { model | previewState = previewState }, Cmd.none )



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



-- VIEW


{-| Whether what the tab shows is the document itself, in its frame. The
project's frame turns its body flush for that and for nothing else: the loader
and the messages want the padding round them.
-}
isDocumentShown : AppState -> Model -> Bool
isDocumentShown appState model =
    case model.previewState of
        Preview (Success urlResponse) ->
            ContentType.isSupportedInBrowser appState.navigator urlResponse.contentType

        _ ->
            False


{-| Exactly one thing, and no heading of its own: the frame above has the
page's `h1`, and the tabs already say which part of the project this is.
-}
view : AppState -> ProjectPreview -> Model -> Html Msg
view appState project model =
    case model.previewState of
        Preview preview ->
            viewPreview appState preview

        PreviewFailed error ->
            DocumentPreviewError.view appState error

        TemplateNotSet ->
            viewTemplateNotSet appState project

        TemplateUnsupported ->
            viewTemplateUnsupported appState project


viewPreview : AppState -> ActionResult UrlResponse -> Html Msg
viewPreview appState preview =
    case preview of
        Unset ->
            Html.text ""

        Loading ->
            -- The wait is the document being rendered, which can take a few
            -- rounds of asking. The same loader as the project's own, so the
            -- two waits read as one.
            Page.loading appState

        Error message ->
            Page.errorWithinPage appState message

        Success urlResponse ->
            viewDocument appState urlResponse


viewDocument : AppState -> UrlResponse -> Html Msg
viewDocument appState urlResponse =
    if ContentType.isSupportedInBrowser appState.navigator urlResponse.contentType then
        iframe
            [ class "c-project-preview"
            , src urlResponse.url

            -- A frame is a document inside a document, and one without a title
            -- is announced as "frame" and skipped.
            , title (gettext "Document preview" appState.locale)
            , TestId.attribute "project-preview_frame"
            ]
            []

    else
        viewNotSupported appState urlResponse.url


viewNotSupported : AppState -> String -> Html Msg
viewNotSupported appState documentUrl =
    Message.message
        { title = gettext "Download preview" appState.locale
        , description = gettext "The document format cannot be displayed in the web browser. You can still download and view it." appState.locale
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.download)
        |> Message.withAction
            (Button.linkPrimary (gettext "Download" appState.locale) documentUrl
                |> Button.withIcon (Icon.solid "fa-download")
                |> Button.external
                |> Button.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                |> Button.withTestId "project-preview_download"
            )
        |> Message.withinPage
        |> Message.toHtml


viewTemplateNotSet : AppState -> ProjectPreview -> Html Msg
viewTemplateNotSet appState project =
    Message.message
        { title = gettext "Default document template is not set." appState.locale
        , description =
            if not (Session.exists appState.session) then
                gettext "Log in to set a default document template and format." appState.locale

            else if ProjectUtils.isOwner appState project then
                gettext "Before you can use preview you need to set a default document template and format." appState.locale

            else
                gettext "Ask the Project owner to set a default document template and format." appState.locale
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.placeholder)
        |> withSettingsAction appState project
        |> Message.withinPage
        |> Message.toHtml


viewTemplateUnsupported : AppState -> ProjectPreview -> Html Msg
viewTemplateUnsupported appState project =
    Message.message
        { title = gettext "Default document template is no longer supported." appState.locale
        , description =
            if not (Session.exists appState.session) then
                gettext "Log in to update the default document template." appState.locale

            else if ProjectUtils.isOwner appState project then
                gettext "Before you can use preview you need to update the default document template." appState.locale

            else
                gettext "Ask the project owner to update the default document template." appState.locale
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.error)
        |> withSettingsAction appState project
        |> Message.withinPage
        |> Message.toHtml


{-| The way to fix it, for the one person who can: the settings tab is where the
template is chosen, and only the owner may open it.
-}
withSettingsAction : AppState -> ProjectPreview -> Message Msg -> Message Msg
withSettingsAction appState project message =
    if Session.exists appState.session && ProjectUtils.isOwner appState project then
        Message.withAction
            (Button.linkPrimary (gettext "Go to settings" appState.locale)
                (Routes.toUrl appState (Routes.projectsDetailSettings project.uuid))
                |> Button.withIcon (Icon.solid "fa-arrow-right")
            )
            message

    else
        message
