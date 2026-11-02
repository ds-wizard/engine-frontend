module Common.Pages.Projects.FileDownload exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| A shareable link that downloads a project file: ask the server for the file's
URL, then send the browser there.

An anonymous visitor who is refused is sent to log in and brought back here;
anyone else who is refused, or asks for a file that does not exist, is told
so on the page.

-}

import ActionResult exposing (ActionResult)
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Api.ProjectFiles as ProjectFilesApi
import Common.Components.Page as Page
import Common.Data.AppState exposing (AppState)
import Common.Data.NotFound as NotFound
import Common.Data.Session as Session
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Illustration as Illustration
import Ui.Page.Message as Message
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Uuid
    , fileUuid : Uuid
    , urlResponse : ActionResult ()
    }


initialModel : Uuid -> Uuid -> Model
initialModel projectUuid fileUuid =
    { projectUuid = projectUuid
    , fileUuid = fileUuid
    , urlResponse = ActionResult.Loading
    }


init : AppState -> Uuid -> Uuid -> ( Model, Cmd Msg )
init appState projectUuid fileUuid =
    ( initialModel projectUuid fileUuid
    , ProjectFilesApi.getFileUrl appState projectUuid fileUuid GotFileUrlCompleted
    )


type Msg
    = GotFileUrlCompleted (Result ApiError UrlResponse)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GotFileUrlCompleted result ->
            case result of
                Ok urlResponse ->
                    ( { model | urlResponse = ActionResult.Success () }, Navigation.load urlResponse.url )

                Err err ->
                    if ApiError.isNotFound err then
                        -- Not an error on this page but a thing that is not
                        -- there: the application says so, in its own words.
                        ( model, UpdateConfig.notFound cfg (NotFound.file appState) )

                    else
                        let
                            redirectCmd =
                                case ( Session.exists appState.session, err ) of
                                    ( False, ApiError.BadStatus 403 _ ) ->
                                        Routes.projectsFileDownload model.projectUuid model.fileUuid
                                            |> Routes.toUrl appState
                                            |> Just
                                            |> Routes.cmdNavigateToLogin appState

                                    _ ->
                                        Cmd.none

                            errorMessage =
                                case err of
                                    ApiError.BadStatus 403 _ ->
                                        gettext "You do not have permission to access this file" appState.locale

                                    _ ->
                                        gettext "Unable to get file" appState.locale
                        in
                        ( { model | urlResponse = ActionResult.Error errorMessage }
                        , redirectCmd
                        )


view : AppState -> Model -> Html msg
view appState model =
    Page.actionResult appState (always (viewDownloading appState)) model.urlResponse


viewDownloading : AppState -> Html msg
viewDownloading appState =
    Message.message
        { title = gettext "Downloading" appState.locale
        , description = gettext "Your download should start shortly." appState.locale
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.download)
        |> Message.toHtml
