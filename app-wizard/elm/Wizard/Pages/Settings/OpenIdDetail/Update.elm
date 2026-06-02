module Wizard.Pages.Settings.OpenIdDetail.Update exposing
    ( UpdateConfig
    , fetchData
    , update
    )

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Ports.Dom as Dom
import Common.Ports.FormUtils as FormUtils
import Common.Ports.Window as Window
import Common.Utils.RequestHelpers as RequestHelpers
import Form
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.OpenIdClients as OpenIdClientsApi
import Wizard.Components.CopyableCodeBlock as CopyableCodeBlock
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.OpenIdClientForm as OpenIdClientForm
import Wizard.Pages.Settings.OpenIdDetail.Models exposing (Model)
import Wizard.Pages.Settings.OpenIdDetail.Msgs exposing (Msg(..))


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    OpenIdClientsApi.getOpenIdClient appState uuid GetOpenIdComplete


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetOpenIdComplete result ->
            case result of
                Ok openId ->
                    ( { model
                        | openIdClient = ActionResult.Success openId
                        , form = OpenIdClientForm.init appState openId
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | openIdClient = ApiError.toActionResult { locale = appState.locale } (gettext "Unable to get OpenID config" appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        PutOpenIdComplete result ->
            let
                ( newResult, cmd ) =
                    case result of
                        Ok _ ->
                            ( ActionResult.Success ()
                            , Window.refresh ()
                            )

                        Err error ->
                            ( ApiError.toActionResult appState (gettext "OpenID config could not be saved." appState.locale) error
                            , RequestHelpers.getResultCmd cfg.logoutMsg result
                            )
            in
            ( { model | savingOpenId = newResult }
            , Cmd.batch [ cmd, Dom.scrollToTop ".container" ]
            )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        newOpenId =
                            OpenIdClientForm.toOpenIdClientDetail model.uuid form

                        cmd =
                            Cmd.map cfg.wrapMsg <|
                                OpenIdClientsApi.putOpenIdClient appState newOpenId PutOpenIdComplete
                    in
                    ( model, cmd )

                _ ->
                    let
                        formRemoved =
                            case formMsg of
                                Form.RemoveItem _ _ ->
                                    True

                                _ ->
                                    model.formRemoved

                        form =
                            Form.update (OpenIdClientForm.validation appState) formMsg model.form
                    in
                    ( { model
                        | form = form
                        , formRemoved = formRemoved
                      }
                    , FormUtils.scrollToInvalidField formMsg
                    )

        CallbackUrlCodeBlockMsg codeBlockMsg ->
            let
                ( callbackUrlCodeBlockState, cmd ) =
                    CopyableCodeBlock.update codeBlockMsg model.callbackUrlCodeBlockState
            in
            ( { model | callbackUrlCodeBlockState = callbackUrlCodeBlockState }
            , Cmd.map (cfg.wrapMsg << CallbackUrlCodeBlockMsg) cmd
            )

        LogoutUrlCodeBlockMsg codeBlockMsg ->
            let
                ( logoutUrlCodeBlockState, cmd ) =
                    CopyableCodeBlock.update codeBlockMsg model.callbackUrlCodeBlockState
            in
            ( { model | callbackUrlCodeBlockState = logoutUrlCodeBlockState }
            , Cmd.map (cfg.wrapMsg << LogoutUrlCodeBlockMsg) cmd
            )

        SetAdvancedConfigExpanded expanded ->
            ( { model | advancedConfigExpanded = expanded }, Cmd.none )
