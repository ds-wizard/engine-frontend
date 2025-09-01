module Wizard.Pages.KnowledgeModelSecrets.Update exposing
    ( UpdateConfig
    , fetchData
    , update
    )

import ActionResult
import Form
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Shared.Data.ApiError as ApiError
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.KnowledgeModelSecrets as KnowledgeModelSecretsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModelSecrets.Forms.KnowledgeModelSecretForm as KnowledgeModelSecretForm
import Wizard.Pages.KnowledgeModelSecrets.Models exposing (Model)
import Wizard.Pages.KnowledgeModelSecrets.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    KnowledgeModelSecretsApi.getKnowledgeModelSecrets appState GetKnowledgeModelSecretsCompleted


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetKnowledgeModelSecretsCompleted result ->
            RequestHelpers.applyResult
                { setResult = \v m -> { m | kmSecrets = v }
                , defaultError = gettext "Unable to load knowledge model secrets." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        SetCreateModalOpen open ->
            ( { model
                | createModalOpen = open
                , createSecretForm = KnowledgeModelSecretForm.initEmpty appState
                , creatingSecret = ActionResult.Unset
              }
            , Cmd.none
            )

        CreateFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.createSecretForm ) of
                ( Form.Submit, Just createForm ) ->
                    ( { model | creatingSecret = ActionResult.Loading }
                    , KnowledgeModelSecretsApi.postKnowledgeModelSecret appState createForm (cfg.wrapMsg << PostKnowledgeModelSecretCompleted)
                    )

                _ ->
                    let
                        updatedForm =
                            Form.update (KnowledgeModelSecretForm.validation appState) formMsg model.createSecretForm
                    in
                    ( { model | createSecretForm = updatedForm }
                    , Cmd.none
                    )

        PostKnowledgeModelSecretCompleted result ->
            case result of
                Ok _ ->
                    ( { model
                        | createModalOpen = False
                        , kmSecrets = ActionResult.Loading
                      }
                    , Cmd.map cfg.wrapMsg (fetchData appState)
                    )

                Err error ->
                    ( { model | creatingSecret = ApiError.toActionResult appState (gettext "Unable to create knowledge model secret." appState.locale) error }
                    , Cmd.none
                    )

        SetEditSecret mbSecret ->
            let
                editSecretForm =
                    Maybe.unwrap model.editSecretForm (KnowledgeModelSecretForm.init appState) mbSecret
            in
            ( { model
                | editSecret = mbSecret
                , editSecretForm = editSecretForm
                , editingSecret = ActionResult.Unset
              }
            , Cmd.none
            )

        EditFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.editSecretForm, model.editSecret ) of
                ( Form.Submit, Just editForm, Just secret ) ->
                    ( { model | editingSecret = ActionResult.Loading }
                    , KnowledgeModelSecretsApi.putKnowledgeModelSecret appState secret.uuid editForm (cfg.wrapMsg << PutKnowledgeModelSecretCompleted)
                    )

                _ ->
                    let
                        updatedForm =
                            Form.update (KnowledgeModelSecretForm.validation appState) formMsg model.editSecretForm
                    in
                    ( { model | editSecretForm = updatedForm }
                    , Cmd.none
                    )

        PutKnowledgeModelSecretCompleted result ->
            case result of
                Ok _ ->
                    ( { model
                        | editSecret = Nothing
                        , kmSecrets = ActionResult.Loading
                      }
                    , Cmd.map cfg.wrapMsg (fetchData appState)
                    )

                Err error ->
                    ( { model | editingSecret = ApiError.toActionResult appState (gettext "Unable to update knowledge model secret." appState.locale) error }
                    , Cmd.none
                    )

        SetDeleteSecret secret ->
            ( { model | deleteSecret = secret, deletingSecret = ActionResult.Unset }
            , Cmd.none
            )

        DeleteKnowledgeModelSecret ->
            case model.deleteSecret of
                Just secret ->
                    ( { model | deletingSecret = ActionResult.Loading }
                    , KnowledgeModelSecretsApi.deleteKnowledgeModelSecret appState secret.uuid (cfg.wrapMsg << DeleteKnowledgeModelSecretCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteKnowledgeModelSecretCompleted result ->
            case result of
                Ok _ ->
                    ( { model
                        | deleteSecret = Nothing
                        , kmSecrets = ActionResult.Loading
                      }
                    , Cmd.map cfg.wrapMsg (fetchData appState)
                    )

                Err error ->
                    ( { model | deletingSecret = ApiError.toActionResult appState (gettext "Unable to delete knowledge model secret." appState.locale) error }
                    , Cmd.none
                    )
