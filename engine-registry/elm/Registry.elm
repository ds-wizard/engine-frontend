module Registry exposing (Model, Msg, PageModel, main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, a, div, img, li, ul)
import Html.Attributes exposing (class, classList, href, src)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Registry.Common.AppState as AppState exposing (AppState)
import Registry.Common.Credentials as Credentials exposing (Credentials)
import Registry.Common.View.Page as Page
import Registry.Pages.ForgottenToken as ForgottenToken
import Registry.Pages.ForgottenTokenConfirmation as ForgottenTokenConfirmation
import Registry.Pages.Index as Index
import Registry.Pages.KMDetail as KMDetail
import Registry.Pages.Login as Login
import Registry.Pages.Organization as Organization
import Registry.Pages.Signup as Signup
import Registry.Pages.SignupConfirmation as SignupConfirmation
import Registry.Pages.TemplateDetail as TemplateDetail
import Registry.Pages.Templates as Templates
import Registry.Ports as Ports
import Registry.Routing as Routing
import Registry.Utils exposing (dispatch)
import Shared.Locale exposing (l, lx)
import Shared.Undraw as Undraw
import Url


l_ : String -> AppState -> String
l_ =
    l "Registry"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Registry"


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkedClicked
        }


type alias Model =
    { route : Routing.Route
    , key : Nav.Key
    , appState : AppState
    , pageModel : PageModel
    }


type PageModel
    = ForgottenTokenModel ForgottenToken.Model
    | ForgottenTokenConfirmationModel ForgottenTokenConfirmation.Model
    | IndexModel Index.Model
    | KMDetailModel KMDetail.Model
    | LoginModel Login.Model
    | OrganizationModel Organization.Model
    | SignupModel Signup.Model
    | SignupConfirmationModel SignupConfirmation.Model
    | TemplatesModel Templates.Model
    | TemplateDetailModel TemplateDetail.Model
    | NotFoundModel


type Msg
    = UrlChanged Url.Url
    | LinkedClicked UrlRequest
    | SetCredentials (Maybe Credentials)
    | ForgottenTokenMsg ForgottenToken.Msg
    | ForgottenTokenConfirmationMsg ForgottenTokenConfirmation.Msg
    | IndexMsg Index.Msg
    | KMDetailMsg KMDetail.Msg
    | LoginMsg Login.Msg
    | OrganizationMsg Organization.Msg
    | SignupMsg Signup.Msg
    | SignupConfirmationMsg SignupConfirmation.Msg
    | TemplatesMsg Templates.Msg
    | TemplateDetailMsg TemplateDetail.Msg


init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    initChildModel
        { route = Routing.toRoute url
        , key = key
        , appState = AppState.init flags
        , pageModel = NotFoundModel
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.pageModel ) of
        ( LinkedClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            initChildModel { model | route = Routing.toRoute url }

        ( SetCredentials mbCredentials, _ ) ->
            let
                ( route, encodedCredentials ) =
                    case mbCredentials of
                        Just credentials ->
                            ( Routing.Organization
                            , Credentials.encode credentials
                            )

                        Nothing ->
                            ( Routing.Index
                            , E.null
                            )
            in
            ( { model | appState = AppState.setCredentials mbCredentials model.appState }
            , Cmd.batch
                [ Nav.pushUrl model.key <| Routing.toString route
                , Ports.saveCredentials encodedCredentials
                ]
            )

        ( ForgottenTokenMsg forgottenTokenMsg, ForgottenTokenModel forgottenTokenModel ) ->
            let
                ( newForgottenTokenModel, forgottenTokenCmd ) =
                    ForgottenToken.update forgottenTokenMsg model.appState forgottenTokenModel
            in
            ( { model | pageModel = ForgottenTokenModel newForgottenTokenModel }
            , Cmd.map ForgottenTokenMsg forgottenTokenCmd
            )

        ( ForgottenTokenConfirmationMsg forgottenTokenConfirmationMsg, ForgottenTokenConfirmationModel forgottenTokenConfirmationModel ) ->
            ( { model | pageModel = ForgottenTokenConfirmationModel <| ForgottenTokenConfirmation.update forgottenTokenConfirmationMsg model.appState forgottenTokenConfirmationModel }
            , Cmd.none
            )

        ( IndexMsg indexMsg, IndexModel indexModel ) ->
            ( { model | pageModel = IndexModel <| Index.update indexMsg model.appState indexModel }
            , Cmd.none
            )

        ( KMDetailMsg kmDetailMsg, KMDetailModel kmDetailModel ) ->
            let
                ( newKmModel, cmd ) =
                    KMDetail.update kmDetailMsg model.appState kmDetailModel
            in
            ( { model | pageModel = KMDetailModel newKmModel }
            , cmd
            )

        ( LoginMsg loginMsg, LoginModel loginModel ) ->
            let
                ( newLoginModel, cmd ) =
                    Login.update
                        { tagger = LoginMsg
                        , loginCmd = \c -> dispatch <| SetCredentials <| Just c
                        }
                        loginMsg
                        model.appState
                        loginModel
            in
            ( { model | pageModel = LoginModel <| newLoginModel }
            , cmd
            )

        ( OrganizationMsg organizationMsg, OrganizationModel organizationModel ) ->
            let
                ( newOrganizationModel, cmd ) =
                    Organization.update organizationMsg model.appState organizationModel
            in
            ( { model | pageModel = OrganizationModel newOrganizationModel }
            , Cmd.map OrganizationMsg cmd
            )

        ( SignupMsg signupMsg, SignupModel signupModel ) ->
            let
                ( newSignupModel, cmd ) =
                    Signup.update signupMsg model.appState signupModel
            in
            ( { model | pageModel = SignupModel newSignupModel }
            , Cmd.map SignupMsg cmd
            )

        ( SignupConfirmationMsg confirmSignupMsg, SignupConfirmationModel signupConfirmationModel ) ->
            ( { model | pageModel = SignupConfirmationModel <| SignupConfirmation.update confirmSignupMsg model.appState signupConfirmationModel }
            , Cmd.none
            )

        ( TemplatesMsg templatesMsg, TemplatesModel templatesModel ) ->
            ( { model | pageModel = TemplatesModel <| Templates.update templatesMsg model.appState templatesModel }
            , Cmd.none
            )

        ( TemplateDetailMsg templateDetailMsg, TemplateDetailModel templateDetailModel ) ->
            let
                ( newTemplateModel, cmd ) =
                    TemplateDetail.update templateDetailMsg model.appState templateDetailModel
            in
            ( { model | pageModel = TemplateDetailModel newTemplateModel }
            , cmd
            )

        _ ->
            ( model, Cmd.none )


initChildModel : Model -> ( Model, Cmd Msg )
initChildModel model =
    case model.route of
        Routing.ForgottenToken ->
            withOrganizationRedirect
                ( { model | pageModel = ForgottenTokenModel ForgottenToken.init }
                , Cmd.none
                )

        Routing.ForgottenTokenConfirmation organizationId hash ->
            let
                ( forgottenTokenConfirmationModel, forgottenTokenConfirmationCmd ) =
                    ForgottenTokenConfirmation.init model.appState organizationId hash
            in
            withOrganizationRedirect
                ( { model | pageModel = ForgottenTokenConfirmationModel forgottenTokenConfirmationModel }
                , Cmd.map ForgottenTokenConfirmationMsg forgottenTokenConfirmationCmd
                )

        Routing.Index ->
            let
                ( indexModel, indexCmd ) =
                    Index.init model.appState
            in
            ( { model | pageModel = IndexModel indexModel }
            , Cmd.map IndexMsg indexCmd
            )

        Routing.KMDetail pkgId ->
            let
                ( kmDetailModel, kmDetailCmd ) =
                    KMDetail.init model.appState pkgId
            in
            ( { model | pageModel = KMDetailModel kmDetailModel }
            , Cmd.map KMDetailMsg kmDetailCmd
            )

        Routing.Login ->
            withOrganizationRedirect
                ( { model | pageModel = LoginModel Login.init }
                , Cmd.none
                )

        Routing.Organization ->
            case model.appState.credentials of
                Just credentials ->
                    let
                        ( organizationModel, organizationCmd ) =
                            Organization.init model.appState credentials
                    in
                    ( { model | pageModel = OrganizationModel organizationModel }
                    , Cmd.map OrganizationMsg organizationCmd
                    )

                Nothing ->
                    ( model
                    , Nav.pushUrl model.key <| Routing.toString Routing.Login
                    )

        Routing.Signup ->
            withOrganizationRedirect
                ( { model | pageModel = SignupModel Signup.init }
                , Cmd.none
                )

        Routing.SignupConfirmation organizationId hash ->
            let
                ( signupConfirmationModel, signupConfirmationCmd ) =
                    SignupConfirmation.init model.appState organizationId hash
            in
            withOrganizationRedirect
                ( { model | pageModel = SignupConfirmationModel signupConfirmationModel }
                , Cmd.map SignupConfirmationMsg signupConfirmationCmd
                )

        Routing.Templates ->
            let
                ( templatesModel, templatesCmd ) =
                    Templates.init model.appState
            in
            ( { model | pageModel = TemplatesModel templatesModel }
            , Cmd.map TemplatesMsg templatesCmd
            )

        Routing.TemplateDetail templateId ->
            let
                ( templateDetailModel, templateDetailCmd ) =
                    TemplateDetail.init model.appState templateId
            in
            ( { model | pageModel = TemplateDetailModel templateDetailModel }
            , Cmd.map TemplateDetailMsg templateDetailCmd
            )

        Routing.NotFound ->
            ( { model | pageModel = NotFoundModel }
            , Cmd.none
            )


withOrganizationRedirect : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withOrganizationRedirect ( model, cmd ) =
    case model.appState.credentials of
        Just _ ->
            ( model, Nav.pushUrl model.key <| Routing.toString Routing.Organization )

        Nothing ->
            ( model, cmd )


view : Model -> Document Msg
view model =
    let
        content =
            if not model.appState.valid then
                Page.illustratedMessage
                    { image = Undraw.bugFixing
                    , heading = l_ "view.invalid.heading" model.appState
                    , msg = l_ "view.invalid.msg" model.appState
                    }

            else
                case model.pageModel of
                    ForgottenTokenModel forgottenTokenModel ->
                        Html.map ForgottenTokenMsg <| ForgottenToken.view model.appState forgottenTokenModel

                    ForgottenTokenConfirmationModel forgottenTokenConfirmationModel ->
                        Html.map ForgottenTokenConfirmationMsg <| ForgottenTokenConfirmation.view model.appState forgottenTokenConfirmationModel

                    IndexModel indexModel ->
                        Html.map IndexMsg <| Index.view indexModel

                    KMDetailModel kmDetailModel ->
                        Html.map KMDetailMsg <| KMDetail.view model.appState kmDetailModel

                    LoginModel loginModel ->
                        Html.map LoginMsg <| Login.view model.appState loginModel

                    OrganizationModel organizationDetailModel ->
                        Html.map OrganizationMsg <| Organization.view model.appState organizationDetailModel

                    SignupModel signupModel ->
                        Html.map SignupMsg <| Signup.view model.appState signupModel

                    SignupConfirmationModel signupConfirmationModel ->
                        Html.map SignupConfirmationMsg <| SignupConfirmation.view model.appState signupConfirmationModel

                    TemplatesModel templatesModel ->
                        Html.map TemplatesMsg <| Templates.view templatesModel

                    TemplateDetailModel templateDetailModel ->
                        Html.map TemplateDetailMsg <| TemplateDetail.view model.appState templateDetailModel

                    NotFoundModel ->
                        Page.illustratedMessage
                            { image = Undraw.pageNotFound
                            , heading = l_ "view.notFound.heading" model.appState
                            , msg = l_ "view.notFound.msg" model.appState
                            }

        html =
            [ header model
            , div [ class "container" ]
                [ content ]
            ]
    in
    { title = l_ "view.title" model.appState
    , body = html
    }


header : Model -> Html Msg
header model =
    let
        appState =
            model.appState

        navigation =
            appState.credentials
                |> Maybe.map (always (loggedInHeaderNavigation appState))
                |> Maybe.withDefault (publicHeaderNavigation appState)
    in
    div [ class "navbar navbar-expand-lg fixed-top navbar-light bg-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", href <| Routing.toString Routing.Index ]
                [ img [ class "logo", src "/img/logo.svg" ] []
                , lx_ "header.brandTitle" appState
                ]
            , ul [ class "nav navbar-nav" ]
                [ li
                    [ class "nav-item"
                    , classList [ ( "active", model.route == Routing.Index ) ]
                    ]
                    [ a [ href <| Routing.toString Routing.Index, class "nav-link" ]
                        [ lx_ "header.knowledgeModels" appState ]
                    ]
                , li
                    [ class "nav-item"
                    , classList [ ( "active", model.route == Routing.Templates ) ]
                    ]
                    [ a [ href <| Routing.toString Routing.Templates, class "nav-link" ]
                        [ lx_ "header.templates" appState ]
                    ]
                ]
            , navigation
            ]
        ]


loggedInHeaderNavigation : AppState -> Html Msg
loggedInHeaderNavigation appState =
    ul [ class "nav navbar-nav ml-auto" ]
        [ li [ class "nav-item" ]
            [ a
                [ href <| Routing.toString Routing.Organization
                , class "nav-link"
                ]
                [ lx_ "loggedInNavigation.profile" appState ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ onClick <| SetCredentials Nothing
                , class "nav-link"
                ]
                [ lx_ "loggedInNavigation.logOut" appState ]
            ]
        ]


publicHeaderNavigation : AppState -> Html Msg
publicHeaderNavigation appState =
    ul [ class "nav navbar-nav ml-auto" ]
        [ li [ class "nav-item" ]
            [ a [ href <| Routing.toString Routing.Login, class "nav-link" ]
                [ lx_ "publicHeaderNavigation.logIn" appState ]
            ]
        , li [ class "nav-item" ]
            [ a [ href <| Routing.toString Routing.Signup, class "nav-link" ]
                [ lx_ "publicHeaderNavigation.signUp" appState ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
