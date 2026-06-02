module Wizard.Pages.Settings.OpenId.Update exposing
    ( UpdateConfig
    , fetchData
    , update
    )

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Wizard.Api.OpenIdClients as OpenIdClientsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.OpenId.Models exposing (Model)
import Wizard.Pages.Settings.OpenId.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    OpenIdClientsApi.getOpenIdClients appState GetOpenIdClientsComplete


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetOpenIdClientsComplete result ->
            RequestHelpers.applyResult
                { setResult = \c m -> { m | openIdClients = c }
                , defaultError = gettext "Unable to get OpenID configs." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        ShowHideDeleteOpenIdClient mbOpenIdClient ->
            ( { model
                | openIdClientToBeDeleted = mbOpenIdClient
                , deletingOpenIdClient = ActionResult.Unset
              }
            , Cmd.none
            )

        DeleteOpenIdClient ->
            case model.openIdClientToBeDeleted of
                Just openId ->
                    ( { model | deletingOpenIdClient = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <|
                        OpenIdClientsApi.deleteOpenIdClient appState openId.uuid DeleteOpenIdClientComplete
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteOpenIdClientComplete result ->
            case result of
                Ok _ ->
                    ( { model
                        | openIdClientToBeDeleted = Nothing
                        , openIdClients = ActionResult.Loading
                      }
                    , OpenIdClientsApi.getOpenIdClients appState (cfg.wrapMsg << GetOpenIdClientsComplete)
                    )

                Err error ->
                    ( { model | deletingOpenIdClient = ApiError.toActionResult appState (gettext "OpenID config could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )
