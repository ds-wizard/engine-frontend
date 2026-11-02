module Common.Pages.Public exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| Public: the feature router for everything before signing in —
log in, sign up and its confirmation, the forgotten password and its
confirmation, the page after logging out, and the OpenID callback.

Public pages render their own frame, because the public template owns the
page's `main` and its title, and only the page knows its title: a form or a
message in a panel over `Common.Pages.Public.Layout.panel`, and a full-page
message or a loading state over `Common.Pages.Public.Layout.app`. The application adds the document title
and the banners around it.

-}

import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Public.ForgottenPassword as ForgottenPassword
import Common.Pages.Public.ForgottenPasswordConfirmation as ForgottenPasswordConfirmation
import Common.Pages.Public.Login as Login
import Common.Pages.Public.LogoutSuccessful as LogoutSuccessful
import Common.Pages.Public.OpenIdCallback as OpenIdCallback
import Common.Pages.Public.Routes as PublicRoutes
import Common.Pages.Public.Signup as Signup
import Common.Pages.Public.SignupConfirmation as SignupConfirmation
import Html exposing (Html)
import Random


type alias Model =
    { openIdCallbackModel : OpenIdCallback.Model
    , forgottenPasswordModel : ForgottenPassword.Model
    , forgottenPasswordConfirmationModel : ForgottenPasswordConfirmation.Model
    , loginModel : Login.Model
    , signupModel : Signup.Model
    , signupConfirmationModel : SignupConfirmation.Model
    }


type Msg
    = OpenIdCallbackMsg OpenIdCallback.Msg
    | ForgottenPasswordMsg ForgottenPassword.Msg
    | ForgottenPasswordConfirmationMsg ForgottenPasswordConfirmation.Msg
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | SignupConfirmationMsg SignupConfirmation.Msg


initialModel : AppState -> Model
initialModel appState =
    { openIdCallbackModel = OpenIdCallback.initialModel "" Nothing Nothing Nothing Nothing
    , forgottenPasswordModel = ForgottenPassword.initialModel
    , forgottenPasswordConfirmationModel = ForgottenPasswordConfirmation.initialModel appState "" ""
    , loginModel = Login.initialModel Nothing
    , signupModel = Signup.initialModel appState
    , signupConfirmationModel = SignupConfirmation.initialModel
    }


init : AppState -> Model -> PublicRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        PublicRoutes.OpenIdCallback id error code mbSessionState mbState ->
            let
                ( openIdCallbackModel, cmd ) =
                    OpenIdCallback.init id error code mbSessionState mbState
            in
            ( { model | openIdCallbackModel = openIdCallbackModel }, Cmd.map OpenIdCallbackMsg cmd )

        PublicRoutes.ForgottenPasswordRoute ->
            ( { model | forgottenPasswordModel = ForgottenPassword.initialModel }, Cmd.none )

        PublicRoutes.ForgottenPasswordConfirmationRoute userId hash ->
            ( { model | forgottenPasswordConfirmationModel = ForgottenPasswordConfirmation.initialModel appState userId hash }, Cmd.none )

        PublicRoutes.LoginRoute mbOriginalUrl ->
            let
                ( loginModel, cmd ) =
                    Login.init appState mbOriginalUrl
            in
            ( { model | loginModel = loginModel }, cmd )

        PublicRoutes.LogoutSuccessful ->
            ( model, LogoutSuccessful.init appState )

        PublicRoutes.SignupRoute ->
            ( { model | signupModel = Signup.initialModel appState }, Cmd.none )

        PublicRoutes.SignupConfirmationRoute userId hash ->
            let
                ( signupConfirmationModel, cmd ) =
                    SignupConfirmation.init appState userId hash
            in
            ( { model | signupConfirmationModel = signupConfirmationModel }, Cmd.map SignupConfirmationMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    let
        ( newModel, cmd ) =
            case msg of
                OpenIdCallbackMsg subMsg ->
                    let
                        ( subModel, subCmd ) =
                            OpenIdCallback.update (UpdateConfig.map OpenIdCallbackMsg cfg) appState subMsg model.openIdCallbackModel
                    in
                    ( { model | openIdCallbackModel = subModel }, subCmd )

                ForgottenPasswordMsg subMsg ->
                    let
                        ( subModel, subCmd ) =
                            ForgottenPassword.update (UpdateConfig.map ForgottenPasswordMsg cfg) appState subMsg model.forgottenPasswordModel
                    in
                    ( { model | forgottenPasswordModel = subModel }, subCmd )

                ForgottenPasswordConfirmationMsg subMsg ->
                    let
                        ( subModel, subCmd ) =
                            ForgottenPasswordConfirmation.update (UpdateConfig.map ForgottenPasswordConfirmationMsg cfg) appState subMsg model.forgottenPasswordConfirmationModel
                    in
                    ( { model | forgottenPasswordConfirmationModel = subModel }, subCmd )

                LoginMsg subMsg ->
                    let
                        ( subModel, subCmd ) =
                            Login.update (UpdateConfig.map LoginMsg cfg) appState subMsg model.loginModel
                    in
                    ( { model | loginModel = subModel }, subCmd )

                SignupMsg subMsg ->
                    let
                        ( subModel, subCmd ) =
                            Signup.update (UpdateConfig.map SignupMsg cfg) appState subMsg model.signupModel
                    in
                    ( { model | signupModel = subModel }, subCmd )

                SignupConfirmationMsg subMsg ->
                    ( { model | signupConfirmationModel = SignupConfirmation.update appState subMsg model.signupConfirmationModel }
                    , Cmd.none
                    )
    in
    ( appState.seed, newModel, cmd )


subscriptions : AppState -> PublicRoutes.Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        PublicRoutes.OpenIdCallback _ _ _ _ _ ->
            Sub.map OpenIdCallbackMsg (OpenIdCallback.subscriptions appState model.openIdCallbackModel)

        PublicRoutes.ForgottenPasswordRoute ->
            Sub.map ForgottenPasswordMsg (ForgottenPassword.subscriptions appState model.forgottenPasswordModel)

        PublicRoutes.ForgottenPasswordConfirmationRoute _ _ ->
            Sub.map ForgottenPasswordConfirmationMsg (ForgottenPasswordConfirmation.subscriptions appState model.forgottenPasswordConfirmationModel)

        PublicRoutes.LoginRoute _ ->
            Sub.map LoginMsg (Login.subscriptions appState model.loginModel)

        PublicRoutes.SignupRoute ->
            Sub.map SignupMsg (Signup.subscriptions appState model.signupModel)

        _ ->
            Sub.none


view : AppState -> PublicRoutes.Route -> Model -> Html Msg
view appState route model =
    case route of
        PublicRoutes.OpenIdCallback _ _ _ _ _ ->
            Html.map OpenIdCallbackMsg (OpenIdCallback.view appState model.openIdCallbackModel)

        PublicRoutes.ForgottenPasswordRoute ->
            Html.map ForgottenPasswordMsg (ForgottenPassword.view appState model.forgottenPasswordModel)

        PublicRoutes.ForgottenPasswordConfirmationRoute _ _ ->
            Html.map ForgottenPasswordConfirmationMsg (ForgottenPasswordConfirmation.view appState model.forgottenPasswordConfirmationModel)

        PublicRoutes.LoginRoute _ ->
            Html.map LoginMsg (Login.view appState model.loginModel)

        PublicRoutes.LogoutSuccessful ->
            LogoutSuccessful.view appState

        PublicRoutes.SignupRoute ->
            Html.map SignupMsg (Signup.view appState model.signupModel)

        PublicRoutes.SignupConfirmationRoute _ _ ->
            Html.map SignupConfirmationMsg (SignupConfirmation.view appState model.signupConfirmationModel)


isGuarded : AppState -> PublicRoutes.Route -> Model -> Maybe String
isGuarded =
    \_ _ _ ->
        Nothing
