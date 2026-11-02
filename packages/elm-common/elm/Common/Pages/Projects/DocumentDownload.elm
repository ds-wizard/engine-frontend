module Common.Pages.Projects.DocumentDownload exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| A shareable link that downloads a document: ask the server for the file's
URL, then send the browser there.

An anonymous visitor who is refused is sent to log in and brought back here;
anyone else who is refused, or asks for a document that does not exist, is told
so on the page.

-}

import ActionResult exposing (ActionResult)
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Documents as DocumentsApi
import Common.Api.Models.UrlResponse exposing (UrlResponse)
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
    , documentUuid : Uuid
    , urlResponse : ActionResult ()
    }


initialModel : Uuid -> Uuid -> Model
initialModel projectUuid documentUuid =
    { projectUuid = projectUuid
    , documentUuid = documentUuid
    , urlResponse = ActionResult.Loading
    }


init : AppState -> Uuid -> Uuid -> ( Model, Cmd Msg )
init appState projectUuid documentUuid =
    ( initialModel projectUuid documentUuid
    , DocumentsApi.getDocumentUrl appState documentUuid GotDocumentUrlCompleted
    )


type Msg
    = GotDocumentUrlCompleted (Result ApiError UrlResponse)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GotDocumentUrlCompleted result ->
            case result of
                Ok urlResponse ->
                    ( { model | urlResponse = ActionResult.Success () }, Navigation.load urlResponse.url )

                Err err ->
                    if ApiError.isNotFound err then
                        -- Not an error on this page but a thing that is not
                        -- there: the application says so, in its own words.
                        ( model, UpdateConfig.notFound cfg (NotFound.document appState) )

                    else
                        let
                            redirectCmd =
                                case ( Session.exists appState.session, err ) of
                                    ( False, ApiError.BadStatus 403 _ ) ->
                                        Routes.projectDocumentDownload model.projectUuid model.documentUuid
                                            |> Routes.toUrl appState
                                            |> Just
                                            |> Routes.cmdNavigateToLogin appState

                                    _ ->
                                        Cmd.none

                            errorMessage =
                                case err of
                                    ApiError.BadStatus 403 _ ->
                                        gettext "You do not have permission to access this document" appState.locale

                                    _ ->
                                        gettext "Unable to get document" appState.locale
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
