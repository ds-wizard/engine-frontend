module Wizard.Pages.Public.OpenIdCallback.Update exposing (fetchData, update)

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Api.Models.Token as Token
import Common.Components.UserExternalCompletionForm as UserExternalCompletionForm
import Common.Ports.LocalStorage as LocalStorage
import Common.Utils.Setters exposing (setAuthenticating, setCompletingRegistration)
import Gettext exposing (gettext)
import Task.Extra as Task
import Wizard.Api.Models.TokenResponse as TokenResponse
import Wizard.Api.OpenIdClients as OpenIdClientsApi
import Wizard.Api.Users as UsersApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Auth.Msgs
import Wizard.Pages.Public.OpenIdCallback.Models exposing (Model)
import Wizard.Pages.Public.OpenIdCallback.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing


fetchData : String -> Maybe String -> Maybe String -> Maybe String -> AppState -> Cmd Msg
fetchData id mbError mbCode mbSessionState appState =
    Cmd.batch
        [ OpenIdClientsApi.getToken appState id mbError mbCode mbSessionState Nothing AuthenticationCompleted
        , LocalStorage.getAndRemoveItem "wizard/originalUrl"
        ]


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    let
        dispatchToken newModel =
            case ActionResult.combine newModel.token newModel.originalUrl of
                ActionResult.Success ( token, originalUrl ) ->
                    ( newModel
                    , Task.dispatch (Wizard.Msgs.AuthMsg <| Wizard.Pages.Auth.Msgs.GotToken token originalUrl)
                    )

                _ ->
                    ( newModel, Cmd.none )

        processTokenResponse result setError =
            case result of
                Ok tokenResponse ->
                    case tokenResponse of
                        TokenResponse.Token token expiresAt ->
                            dispatchToken { model | token = ActionResult.Success (Token.create token expiresAt) }

                        TokenResponse.ConsentsRequired hash ->
                            ( { model | hash = Just hash }, Cmd.none )

                        TokenResponse.IdentityLinked ->
                            ( model, Routing.cmdNavigate appState Routes.usersEditConnectedAccounts )

                        TokenResponse.CompleteRegistrationRequired userFromExternal ->
                            ( { model | completionForm = Just (UserExternalCompletionForm.init userFromExternal) }
                            , Cmd.none
                            )

                        TokenResponse.EmailVerificationRequired ->
                            ( { model | emailVerificationRequired = True }, Cmd.none )

                        _ ->
                            ( setError (ActionResult.Error (gettext "Unexpected response from the server." appState.locale)) model
                            , Cmd.none
                            )

                Err error ->
                    ( setError (ApiError.toActionResult appState (gettext "Login failed." appState.locale) error) model
                    , Cmd.none
                    )
    in
    case msg of
        GotOriginalUrl localStorageItemResult ->
            case localStorageItemResult of
                Ok localStorageData ->
                    if localStorageData.key == "wizard/originalUrl" then
                        dispatchToken { model | originalUrl = ActionResult.Success localStorageData.value }

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AuthenticationCompleted result ->
            processTokenResponse result setAuthenticating

        CheckConsent value ->
            ( { model | consent = value }, Cmd.none )

        SubmitConsent ->
            case model.hash of
                Just hash ->
                    let
                        cmd =
                            Cmd.map wrapMsg (UsersApi.postConsents appState hash model.sessionState SubmitConsentCompleted)
                    in
                    ( { model | submittingConsent = ActionResult.Loading }
                    , cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        SubmitConsentCompleted result ->
            case result of
                Ok tokenResponse ->
                    case tokenResponse of
                        TokenResponse.Token token expiresAt ->
                            ( model, Task.dispatch (Wizard.Msgs.AuthMsg <| Wizard.Pages.Auth.Msgs.GotToken (Token.create token expiresAt) Nothing) )

                        _ ->
                            ( { model | submittingConsent = ActionResult.Error (gettext "Unexpected response from the server." appState.locale) }, Cmd.none )

                Err error ->
                    ( { model | submittingConsent = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }, Cmd.none )

        CompletionFormMsg completionFormMsg ->
            case model.completionForm of
                Just completionFormModel ->
                    let
                        updateConfig =
                            { submitMsg = Task.dispatch << SubmitCompletionForm
                            , wrapMsg = CompletionFormMsg
                            }

                        ( updatedCompletionFormModel, completionFormCmd ) =
                            UserExternalCompletionForm.update updateConfig completionFormMsg completionFormModel
                    in
                    ( { model | completionForm = Just updatedCompletionFormModel }
                    , Cmd.map wrapMsg completionFormCmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        SubmitCompletionForm userFromExternal ->
            ( { model | completingRegistration = ActionResult.Loading }
            , Cmd.map wrapMsg (UsersApi.postFromExternal appState userFromExternal SubmitCompletionFormCompleted)
            )

        SubmitCompletionFormCompleted result ->
            processTokenResponse result setCompletingRegistration
