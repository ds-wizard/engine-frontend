module Common.Pages.Public.Login exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Log in: email and password, then a code from an email when the account
has two-factor authentication, or an external identity provider.

The code step's Verify button is not disabled while the code is not a number:
the design system never disables a submit on validity. A
submit with no valid code says so on the field instead, and sends nothing.

A client can add ways to sign in and a captcha (`Common.Data.Extensions`). An
added service is a button after the OpenID ones that keeps the address the
visitor came from, as they do, and leaves for the service. With the captcha on,
the first Log In replaces the fields with the challenge, and solving it sends
the email and password with its token.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Common.Api.Models.OpenIdRequest exposing (OpenIdRequestResponse)
import Common.Api.Models.Token as Token
import Common.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Common.Api.OpenIdClients as OpenIdClientApi
import Common.Api.Tokens as TokensApi
import Common.Components.Announcements as Announcements
import Common.Components.ExternalLoginButton as ExternalLoginButton
import Common.Components.Form as Form
import Common.Data.AppState exposing (AppState)
import Common.Data.Session as Session
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Public.Layout as PublicLayout
import Common.Ports.Captcha as Captcha
import Common.Ports.Dom as Dom
import Common.Ports.LocalStorage as LocalStorage
import Common.Routes as Routes
import Common.Utils.MarkdownOrHtml as MarkdownOrHtml
import Gettext exposing (gettext)
import Html exposing (Html)
import Html.Attributes exposing (id)
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import String.Extra as String
import Task.Extra as Task
import Ui.Button as Button
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Prose as Prose
import Ui.Separator as Separator


type alias Model =
    { email : String
    , password : String
    , code : String
    , loggingIn : ActionResult String
    , originalUrl : Maybe String
    , codeRequired : Bool
    , codeSubmitted : Bool
    , adminLoginVisible : Bool
    , captchaShown : Bool
    }


initialModel : Maybe String -> Model
initialModel mbOriginalUrl =
    { email = ""
    , password = ""
    , code = ""
    , loggingIn = Unset
    , originalUrl = mbOriginalUrl
    , codeRequired = False
    , codeSubmitted = False
    , adminLoginVisible = False
    , captchaShown = False
    }


type Msg
    = Email String
    | Password String
    | Code String
    | DoLogin
    | LoginCompleted (Result ApiError TokenResponse)
    | ExternalLoginOpenId OpenIDServiceConfig
    | ExternalLoginOpenIdCompleted (Result ApiError OpenIdRequestResponse)
    | ShowAdminLogin
    | ExternalLoginRedirect String
    | GotCaptchaToken String


init : AppState -> Maybe String -> ( Model, Cmd msg )
init appState mbOriginalUrl =
    let
        cmd =
            if Session.isValid appState.currentTime appState.session then
                Routes.navigate appState Routes.appHome

            else
                Cmd.none
    in
    ( initialModel mbOriginalUrl, cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Code code ->
            ( { model | code = code }, Cmd.none )

        DoLogin ->
            if model.codeRequired && not (codeValid model) then
                ( { model | codeSubmitted = True }, Dom.focus ("#" ++ codeFieldId) )

            else if appState.extensions.loginCaptcha && not model.codeRequired && not model.captchaShown then
                -- The fields make way for the challenge; solving it logs in.
                ( { model | captchaShown = True, loggingIn = Loading }
                , Captcha.initCaptcha ("#" ++ captchaElementId)
                )

            else
                doLogin cfg appState Nothing model

        GotCaptchaToken captchaToken ->
            doLogin cfg appState (Just captchaToken) model

        ExternalLoginRedirect url ->
            ( model
            , Cmd.batch
                [ Navigation.load url
                , saveLocalStorageOptionalItem "wizard/originalUrl" model.originalUrl
                ]
            )

        LoginCompleted result ->
            loginCompleted cfg appState model result

        ExternalLoginOpenId openIdServiceConfig ->
            ( model
            , OpenIdClientApi.request appState openIdServiceConfig (cfg.wrapMsg << ExternalLoginOpenIdCompleted)
            )

        ExternalLoginOpenIdCompleted result ->
            case result of
                Ok openIdRequestResponse ->
                    ( model
                    , Cmd.batch
                        [ Navigation.load openIdRequestResponse.url
                        , saveLocalStorageOptionalItem "wizard/originalUrl" model.originalUrl
                        , saveLocalStorageOptionalItem "wizard/state" (Just openIdRequestResponse.state)
                        ]
                    )

                Err error ->
                    ( { model | loggingIn = ApiError.toActionResult appState (gettext "External login failed." appState.locale) error }
                    , Form.focusErrorSummary
                    )

        ShowAdminLogin ->
            -- The button that showed the form is gone; the form's first field is where the reader wanted to be.
            ( { model | adminLoginVisible = True }, Dom.focus ("#" ++ emailFieldId) )


doLogin : UpdateConfig Msg msg -> AppState -> Maybe String -> Model -> ( Model, Cmd msg )
doLogin cfg appState mbCaptchaToken model =
    let
        body =
            E.object
                [ ( "email", E.string model.email )
                , ( "password", E.string model.password )
                , ( "code", E.maybe E.int (Maybe.andThen String.toInt (String.toMaybe model.code)) )
                ]
    in
    ( { model | loggingIn = Loading }
    , Cmd.map cfg.wrapMsg <| TokensApi.fetchTokenWithCaptcha appState mbCaptchaToken body LoginCompleted
    )


loginCompleted : UpdateConfig Msg msg -> AppState -> Model -> Result ApiError TokenResponse -> ( Model, Cmd msg )
loginCompleted cfg appState model result =
    case result of
        Ok tokenResponse ->
            case tokenResponse of
                TokenResponse.Token token expiresAt ->
                    ( model, Task.dispatch (cfg.gotTokenMsg (Token.create token expiresAt) model.originalUrl) )

                TokenResponse.CodeRequired ->
                    -- The email and password fields are replaced by the code field, so focus follows.
                    ( { model | codeRequired = True, loggingIn = Unset }, Dom.focus ("#" ++ codeFieldId) )

                TokenResponse.ConsentsRequired _ ->
                    ( model, Cmd.none )

                TokenResponse.IdentityLinked ->
                    ( model, Navigation.load (Routes.toUrl appState Routes.usersEditConnectedAccounts) )

                _ ->
                    ( { model | loggingIn = ActionResult.Error (gettext "Unexpected response from the server." appState.locale) }
                    , Form.focusErrorSummary
                    )

        Err error ->
            -- A failed attempt asks for a new captcha, as the fields come back.
            ( { model
                | loggingIn = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error
                , captchaShown = False
              }
            , Form.focusErrorSummary
            )


saveLocalStorageOptionalItem : String -> Maybe String -> Cmd msg
saveLocalStorageOptionalItem key mbValue =
    case mbValue of
        Just value ->
            LocalStorage.setItem key (E.string value)

        Nothing ->
            Cmd.none


codeValid : Model -> Bool
codeValid model =
    Maybe.isJust (String.toInt model.code)


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if model.captchaShown then
        Captcha.onCaptchaSuccess GotCaptchaToken

    else if not (loginFormVisible appState model || model.codeRequired) then
        Sub.none

    else
        Form.submitShortcut appState { result = model.loggingIn, onSubmit = DoLogin }



-- VIEW


emailFieldId : String
emailFieldId =
    "login-email"


codeFieldId : String
codeFieldId =
    "login-code"


captchaElementId : String
captchaElementId =
    "login-captcha"


loginFormVisible : AppState -> Model -> Bool
loginFormVisible appState model =
    appState.config.authentication.nonAdminLoginEnabled || model.adminLoginVisible


view : AppState -> Model -> Html Msg
view appState model =
    let
        loginScreen =
            appState.config.loginScreen

        viewInfo info =
            Prose.toHtml [ MarkdownOrHtml.toHtml [] info ]

        form =
            if model.codeRequired then
                codeFormView appState model

            else
                loginFormView appState model
    in
    PublicLayout.panel appState
        { title = gettext "Log In" appState.locale
        , content = [ Announcements.view loginScreen.announcements, form ]
        , intro = Maybe.map viewInfo loginScreen.loginInfo
        , aside = Maybe.map viewInfo loginScreen.loginInfoSidebar
        }


loginFormView : AppState -> Model -> Html Msg
loginFormView appState model =
    let
        externalLoginButtons =
            List.map
                (\service -> ExternalLoginButton.view { onClick = ExternalLoginOpenId service, service = service })
                appState.config.authentication.services
                ++ List.map
                    (\service -> ExternalLoginButton.view { onClick = ExternalLoginRedirect service.url, service = service })
                    appState.extensions.loginServices
    in
    if loginFormVisible appState model then
        let
            externalLogin =
                if List.isEmpty externalLoginButtons then
                    []

                else
                    [ Separator.labeled (gettext "Or connect with" appState.locale)
                    , Layout.stack externalLoginButtons
                    ]
        in
        Layout.stack
            (Form.view
                { onSubmit = DoLogin
                , content =
                    if model.captchaShown then
                        -- The challenge draws itself here and sends the form on when it is solved.
                        [ Form.errorSummary model.loggingIn
                        , Html.div [ id captchaElementId ] []
                        ]

                    else
                        credentialFields appState model
                }
                :: externalLogin
            )

    else
        Layout.stack
            [ Form.errorSummary model.loggingIn
            , Layout.stack externalLoginButtons
            , Layout.inline
                [ Button.iconOnly (Icon.solid "fa-user-shield") (gettext "Login as admin" appState.locale) ShowAdminLogin
                    |> Button.toHtml
                ]
            ]


credentialFields : AppState -> Model -> List (Html Msg)
credentialFields appState model =
    [ Form.errorSummary model.loggingIn
    , Field.input { id = emailFieldId, label = gettext "Email" appState.locale, value = model.email, onInput = Email }
        |> Field.withAutocomplete "username"
        |> Field.withInputMode "email"
        |> Field.toHtml
    , Field.password { id = "login-password", label = gettext "Password" appState.locale, value = model.password, onInput = Password }
        |> Field.withAutocomplete "current-password"
        |> Field.toHtml
    , -- One row, as the wizard had it, with the button first: it is
      -- what follows the password in the tab order, and the link
      -- is the way out for someone who cannot use it. The row
      -- wraps when the panel is too narrow for both.
      Layout.inlineBetween
        [ Button.submit (gettext "Log In" appState.locale)
            |> Button.withLoading (ActionResult.isLoading model.loggingIn)
            |> Button.toHtml
        , Link.link { label = gettext "Forgot your password?" appState.locale, url = Routes.toUrl appState Routes.publicForgottenPassword }
            |> Link.toHtml
        ]
    ]


codeFormView : AppState -> Model -> Html Msg
codeFormView appState model =
    let
        codeError =
            if model.codeSubmitted && not (codeValid model) then
                Just (gettext "This is not a valid number." appState.locale)

            else
                Nothing
    in
    Form.view
        { onSubmit = DoLogin
        , content =
            [ Form.errorSummary model.loggingIn
            , Field.input { id = codeFieldId, label = gettext "Authentication Code" appState.locale, value = model.code, onInput = Code }
                |> Field.withHint (gettext "Please enter the authentication code from your email to verify your identity." appState.locale)
                |> Field.withAutocomplete "one-time-code"
                |> Field.withInputMode "numeric"
                |> Field.withError codeError
                |> Field.toHtml
            , Form.actions appState
                { submitLabel = gettext "Verify" appState.locale
                , result = model.loggingIn
                , onCancel = Nothing
                }
            ]
        }
