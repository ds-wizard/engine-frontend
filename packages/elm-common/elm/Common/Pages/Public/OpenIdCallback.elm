module Common.Pages.Public.OpenIdCallback exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Where an external identity provider sends someone back after they signed in
there.

The page reads the address the visitor started from and the state it sent
along (both kept in local storage by the login page), checks the state, and
exchanges the provider's answer for a token. The server may want more first:
consent to the instance's privacy policy and terms, the details the provider did
not send, or a verified email address.

The consent step's Continue is not disabled while the box is unticked, as the
wizard's was: the design system never disables a submit on validity. Continue
with the box unticked says so on the checkbox instead, and sends nothing.

-}

import ActionResult
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Token as Token exposing (Token)
import Common.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Common.Api.Models.UserFromExternal exposing (UserFromExternal)
import Common.Api.OpenIdClients as OpenIdClientsApi
import Common.Api.Users as UsersApi
import Common.Components.Form as Form
import Common.Components.Page as Page
import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Public.Components.Consent as Consent
import Common.Pages.Public.Components.UserExternalCompletionForm as UserExternalCompletionForm
import Common.Pages.Public.Layout as PublicLayout
import Common.Ports.Dom as Dom
import Common.Ports.LocalStorage as LocalStorage
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)
import Json.Decode as D
import Maybe.Extra as Maybe
import Task.Extra as Task
import Ui.AppShell as AppShell
import Ui.Illustration as Illustration
import Ui.Page.Message as Message


type alias Model =
    { id : String
    , error : Maybe String
    , code : Maybe String
    , sessionState : Maybe String
    , state : Maybe String
    , consent : Bool
    , consentSubmitted : Bool
    , authenticating : ActionResult.ActionResult String
    , hash : Maybe String
    , submittingConsent : ActionResult.ActionResult String
    , originalUrl : ActionResult.ActionResult (Maybe String)
    , originalState : ActionResult.ActionResult (Maybe String)
    , token : ActionResult.ActionResult Token
    , completionForm : Maybe UserExternalCompletionForm.Model
    , completingRegistration : ActionResult.ActionResult String
    , emailVerificationRequired : Bool
    }


initialModel : String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Model
initialModel id error code sessionState state =
    { id = id
    , error = error
    , code = code
    , sessionState = sessionState
    , state = state
    , consent = False
    , consentSubmitted = False
    , authenticating = ActionResult.Loading
    , hash = Nothing
    , submittingConsent = ActionResult.Unset
    , originalUrl = ActionResult.Loading
    , originalState = ActionResult.Loading
    , token = ActionResult.Loading
    , completionForm = Nothing
    , completingRegistration = ActionResult.Unset
    , emailVerificationRequired = False
    }


type Msg
    = GotOriginalUrl (Result D.Error (LocalStorage.Item (Maybe String)))
    | AuthenticationCompleted (Result ApiError TokenResponse)
    | CheckConsent Bool
    | SubmitConsent
    | SubmitConsentCompleted (Result ApiError TokenResponse)
    | CompletionFormMsg UserExternalCompletionForm.Msg
    | SubmitCompletionForm UserFromExternal
    | SubmitCompletionFormCompleted (Result ApiError TokenResponse)


init : String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
init id error code sessionState state =
    ( initialModel id error code sessionState state
    , Cmd.batch
        [ LocalStorage.getAndRemoveItem "wizard/originalUrl"
        , LocalStorage.getAndRemoveItem "wizard/state"
        ]
    )


consentFieldId : String
consentFieldId =
    "open-id-consent"


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        requestState newModel =
            case newModel.originalState of
                ActionResult.Success originalState ->
                    if newModel.state /= originalState then
                        ( { newModel | authenticating = ActionResult.Error (gettext "Your sign-in attempt has expired or was started in a different window. Please try signing in again." appState.locale) }
                        , focusTitle
                        )

                    else
                        ( newModel
                        , OpenIdClientsApi.getToken appState newModel.id model.error model.code newModel.sessionState newModel.state (cfg.wrapMsg << AuthenticationCompleted)
                        )

                _ ->
                    ( newModel, Cmd.none )

        dispatchToken newModel =
            case ActionResult.combine newModel.token newModel.originalUrl of
                ActionResult.Success ( token, originalUrl ) ->
                    ( newModel, Task.dispatch (cfg.gotTokenMsg token originalUrl) )

                _ ->
                    ( newModel, Cmd.none )

        processTokenResponse result setError errorFocus =
            case result of
                Ok tokenResponse ->
                    case tokenResponse of
                        TokenResponse.Token token expiresAt ->
                            dispatchToken { model | token = ActionResult.Success (Token.create token expiresAt) }

                        TokenResponse.ConsentsRequired hash ->
                            ( { model | hash = Just hash }, focusTitle )

                        TokenResponse.IdentityLinked ->
                            ( model, Routes.navigate appState Routes.usersEditConnectedAccounts )

                        TokenResponse.CompleteRegistrationRequired userFromExternal ->
                            ( { model | completionForm = Just (UserExternalCompletionForm.init userFromExternal) }
                            , focusTitle
                            )

                        TokenResponse.EmailVerificationRequired ->
                            ( { model | emailVerificationRequired = True }, focusTitle )

                        _ ->
                            ( setError (ActionResult.Error (gettext "Unexpected response from the server." appState.locale)) model
                            , errorFocus
                            )

                Err error ->
                    ( setError (ApiError.toActionResult appState (gettext "Login failed." appState.locale) error) model
                    , errorFocus
                    )
    in
    case msg of
        GotOriginalUrl localStorageItemResult ->
            case localStorageItemResult of
                Ok localStorageData ->
                    if localStorageData.key == "wizard/originalUrl" then
                        dispatchToken { model | originalUrl = ActionResult.Success localStorageData.value }

                    else if localStorageData.key == "wizard/state" then
                        requestState { model | originalState = ActionResult.Success localStorageData.value }

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AuthenticationCompleted result ->
            processTokenResponse result (\value m -> { m | authenticating = value }) focusTitle

        CheckConsent value ->
            ( { model | consent = value }, Cmd.none )

        SubmitConsent ->
            case model.hash of
                Just hash ->
                    if model.consent || not (Consent.isRequired appState) then
                        ( { model | submittingConsent = ActionResult.Loading }
                        , Cmd.map cfg.wrapMsg (UsersApi.postConsents appState hash model.sessionState SubmitConsentCompleted)
                        )

                    else
                        ( { model | consentSubmitted = True }, Dom.focus ("#" ++ consentFieldId) )

                Nothing ->
                    ( model, Cmd.none )

        SubmitConsentCompleted result ->
            case result of
                Ok tokenResponse ->
                    case tokenResponse of
                        TokenResponse.Token token expiresAt ->
                            ( model, Task.dispatch (cfg.gotTokenMsg (Token.create token expiresAt) Nothing) )

                        _ ->
                            ( { model | submittingConsent = ActionResult.Error (gettext "Unexpected response from the server." appState.locale) }
                            , Form.focusErrorSummary
                            )

                Err error ->
                    ( { model | submittingConsent = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }
                    , Form.focusErrorSummary
                    )

        CompletionFormMsg completionFormMsg ->
            case model.completionForm of
                Just completionFormModel ->
                    let
                        ( updatedCompletionFormModel, completionFormCmd ) =
                            UserExternalCompletionForm.update
                                { submitMsg = Task.dispatch << SubmitCompletionForm
                                , wrapMsg = CompletionFormMsg
                                }
                                completionFormMsg
                                completionFormModel
                    in
                    ( { model | completionForm = Just updatedCompletionFormModel }
                    , Cmd.map cfg.wrapMsg completionFormCmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        SubmitCompletionForm userFromExternal ->
            ( { model | completingRegistration = ActionResult.Loading }
            , Cmd.map cfg.wrapMsg (UsersApi.postFromExternal appState userFromExternal SubmitCompletionFormCompleted)
            )

        SubmitCompletionFormCompleted result ->
            processTokenResponse result (\value m -> { m | completingRegistration = value }) Form.focusErrorSummary


{-| What was on the screen has been replaced by a page with a new heading.
-}
focusTitle : Cmd msg
focusTitle =
    Dom.focus ("#" ++ AppShell.mainHeadingId)


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    let
        shortcut =
            if model.emailVerificationRequired then
                Sub.none

            else if Maybe.isJust model.hash then
                Form.submitShortcut appState { result = model.submittingConsent, onSubmit = SubmitConsent }

            else if Maybe.isJust model.completionForm then
                Form.submitShortcut appState
                    { result = model.completingRegistration
                    , onSubmit = CompletionFormMsg UserExternalCompletionForm.submitMsg
                    }

            else
                Sub.none
    in
    Sub.batch
        [ LocalStorage.gotItem (D.maybe D.string) GotOriginalUrl
        , shortcut
        ]


view : AppState -> Model -> Html Msg
view appState model =
    if model.emailVerificationRequired then
        PublicLayout.app appState
            [ Message.message
                { title = gettext "Sign up was successful" appState.locale
                , description = gettext "Check your email for the activation link." appState.locale
                }
                |> Message.withIllustration (Illustration.toHtml Illustration.success)
                |> Message.toHtml
            ]

    else if Maybe.isJust model.hash then
        PublicLayout.panel appState
            { title = gettext "Consent Confirmation" appState.locale
            , intro = Nothing
            , aside = Nothing
            , content =
                [ Form.view
                    { onSubmit = SubmitConsent
                    , content =
                        [ Form.errorSummary model.submittingConsent
                        , Consent.field appState
                            { id = consentFieldId
                            , checked = model.consent
                            , onCheck = CheckConsent
                            , invalid = model.consentSubmitted && not model.consent
                            , onFocus = Nothing
                            , onBlur = Nothing
                            }
                        , Form.actions appState
                            { submitLabel = gettext "Continue" appState.locale
                            , result = model.submittingConsent
                            , onCancel = Nothing
                            }
                        ]
                    }
                ]
            }

    else
        case model.completionForm of
            Just completionFormModel ->
                PublicLayout.panel appState
                    { title = gettext "Complete Your Registration" appState.locale
                    , intro = Nothing
                    , aside = Nothing
                    , content =
                        [ Html.map CompletionFormMsg <|
                            UserExternalCompletionForm.view appState
                                { completingRegistration = model.completingRegistration }
                                completionFormModel
                        ]
                    }

            Nothing ->
                PublicLayout.app appState
                    [ Page.actionResult appState (always (Html.text "")) model.authenticating ]
