module Registry exposing (Model, Msg, main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Gettext exposing (gettext)
import Html
import Json.Decode as D
import Registry.Components.AboutModal as AboutModal
import Registry.Components.Page as Page
import Registry.Data.AppState as AppState exposing (AppState)
import Registry.Data.Session as Session exposing (Session)
import Registry.Layouts.AppLayout as Layout
import Registry.Pages.DocumentTemplates as DocumentTemplates
import Registry.Pages.DocumentTemplatesDetail as DocumentTemplatesDetail
import Registry.Pages.ForgottenToken as ForgottenToken
import Registry.Pages.ForgottenTokenConfirmation as ForgottenTokenConfirmation
import Registry.Pages.Homepage as Homepage
import Registry.Pages.KnowledgeModels as KnowledgeModels
import Registry.Pages.KnowledgeModelsDetail as KnowledgeModelsDetail
import Registry.Pages.Locales as Locales
import Registry.Pages.LocalesDetail as LocalesDetail
import Registry.Pages.Login as Login
import Registry.Pages.OrganizationDetail as OragnizationDetail
import Registry.Pages.Signup as Signup
import Registry.Pages.SignupConfirmation as SignupConfirmation
import Registry.Ports as Ports
import Registry.Routes as Routes
import Shared.Undraw as Undraw
import Task
import Task.Extra as Task
import Time
import Url exposing (Url)


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


type alias Model =
    { appState : AppState
    , menuVisible : Bool
    , pages :
        { knowledgeModels : KnowledgeModels.Model
        , knowledgeModelsDetail : KnowledgeModelsDetail.Model
        , documentTemplates : DocumentTemplates.Model
        , documentTemplatesDetail : DocumentTemplatesDetail.Model
        , locales : Locales.Model
        , localesDetail : LocalesDetail.Model
        , login : Login.Model
        , signup : Signup.Model
        , signupConfirmation : SignupConfirmation.Model
        , forgottenToken : ForgottenToken.Model
        , forgottenTokenConfirmation : ForgottenTokenConfirmation.Model
        , organizationDetail : OragnizationDetail.Model
        }
    , aboutModalModel : AboutModal.Model
    }


setRoute : Routes.Route -> Model -> Model
setRoute route model =
    let
        appState =
            model.appState
    in
    { model | appState = { appState | route = route } }


init : D.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( appState, cmd ) =
            case AppState.init flags key of
                Just appStateWithoutRoute ->
                    case Routes.redirect url of
                        Just redirectUrl ->
                            ( appStateWithoutRoute, Navigation.pushUrl key (Url.toString redirectUrl) )

                        Nothing ->
                            let
                                originalRoute =
                                    Routes.parse appStateWithoutRoute.config url
                            in
                            ( { appStateWithoutRoute | route = routeIfAllowed appStateWithoutRoute originalRoute }
                            , Task.dispatch (OnUrlChange url)
                            )

                Nothing ->
                    ( AppState.default key, Cmd.none )

        model =
            { appState = appState
            , menuVisible = False
            , pages =
                { knowledgeModels = KnowledgeModels.initialModel
                , knowledgeModelsDetail = KnowledgeModelsDetail.initialModel
                , documentTemplates = DocumentTemplates.initialModel
                , documentTemplatesDetail = DocumentTemplatesDetail.initialModel
                , locales = Locales.initialModel
                , localesDetail = LocalesDetail.initialModel
                , login = Login.initialModel
                , signup = Signup.initialModel appState
                , signupConfirmation = SignupConfirmation.initialModel
                , forgottenToken = ForgottenToken.initialModel
                , forgottenTokenConfirmation = ForgottenTokenConfirmation.initialModel
                , organizationDetail = OragnizationDetail.initialModel
                }
            , aboutModalModel = AboutModal.initialModel
            }
    in
    ( model, Cmd.batch [ cmd, Task.perform OnTimeZone Time.here ] )


routeIfAllowed : AppState -> Routes.Route -> Routes.Route
routeIfAllowed appState route =
    if Routes.isAllowed appState route then
        route

    else
        Routes.NotAllowed


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnTimeZone Time.Zone
    | SetMenuVisible Bool
    | SetSession (Maybe Session)
    | PagesKnowledgeModelsMsg KnowledgeModels.Msg
    | PagesKnowledgeModelsDetailMsg KnowledgeModelsDetail.Msg
    | PagesDocumentTemplatesMsg DocumentTemplates.Msg
    | PagesDocumentTemplatesDetailMsg DocumentTemplatesDetail.Msg
    | PagesLocalesMsg Locales.Msg
    | PagesLocalesDetailMsg LocalesDetail.Msg
    | PagesLoginMsg Login.Msg
    | PagesSignupMsg Signup.Msg
    | PagesSignupConfirmationMsg SignupConfirmation.Msg
    | PagesForgottenTokenMsg ForgottenToken.Msg
    | PagesForgottenTokenConfirmationMsg ForgottenTokenConfirmation.Msg
    | PagesOrganizationDetailMsg OragnizationDetail.Msg
    | AboutModalMsg AboutModal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange url ->
            let
                nextRoute =
                    routeIfAllowed model.appState (Routes.parse model.appState.config url)
            in
            initPage (setRoute nextRoute model)

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | menuVisible = False }
                    , Navigation.pushUrl model.appState.key (Url.toString url)
                    )

                Browser.External url ->
                    if url == "" then
                        ( model, Cmd.none )

                    else
                        ( model, Navigation.load url )

        OnTimeZone timeZone ->
            ( { model | appState = AppState.setTimeZone timeZone model.appState }, Cmd.none )

        SetMenuVisible visible ->
            ( { model | menuVisible = visible }, Cmd.none )

        SetSession session ->
            let
                sessionCmd =
                    case session of
                        Just s ->
                            Ports.saveSession (Session.encode s)

                        Nothing ->
                            Ports.clearSession ()
            in
            ( { model | appState = AppState.setSession session model.appState }
            , Cmd.batch
                [ Routes.navigate model.appState.key Routes.home
                , sessionCmd
                ]
            )

        PagesKnowledgeModelsMsg pageMsg ->
            let
                ( knowledgeModels, cmd ) =
                    KnowledgeModels.update model.appState pageMsg model.pages.knowledgeModels

                pages =
                    model.pages
            in
            ( { model | pages = { pages | knowledgeModels = knowledgeModels } }
            , Cmd.map PagesKnowledgeModelsMsg cmd
            )

        PagesKnowledgeModelsDetailMsg pageMsg ->
            let
                ( knowledgeModelsDetail, cmd ) =
                    KnowledgeModelsDetail.update model.appState pageMsg model.pages.knowledgeModelsDetail

                pages =
                    model.pages
            in
            ( { model | pages = { pages | knowledgeModelsDetail = knowledgeModelsDetail } }
            , Cmd.map PagesKnowledgeModelsDetailMsg cmd
            )

        PagesDocumentTemplatesMsg pageMsg ->
            let
                ( documentTemplates, cmd ) =
                    DocumentTemplates.update model.appState pageMsg model.pages.documentTemplates

                pages =
                    model.pages
            in
            ( { model | pages = { pages | documentTemplates = documentTemplates } }
            , Cmd.map PagesDocumentTemplatesMsg cmd
            )

        PagesDocumentTemplatesDetailMsg pageMsg ->
            let
                ( documentTemplatesDetail, cmd ) =
                    DocumentTemplatesDetail.update model.appState pageMsg model.pages.documentTemplatesDetail

                pages =
                    model.pages
            in
            ( { model | pages = { pages | documentTemplatesDetail = documentTemplatesDetail } }
            , Cmd.map PagesDocumentTemplatesDetailMsg cmd
            )

        PagesLocalesMsg pageMsg ->
            let
                ( locales, cmd ) =
                    Locales.update model.appState pageMsg model.pages.locales

                pages =
                    model.pages
            in
            ( { model | pages = { pages | locales = locales } }
            , Cmd.map PagesLocalesMsg cmd
            )

        PagesLocalesDetailMsg pageMsg ->
            let
                ( localesDetail, cmd ) =
                    LocalesDetail.update model.appState pageMsg model.pages.localesDetail

                pages =
                    model.pages
            in
            ( { model | pages = { pages | localesDetail = localesDetail } }
            , Cmd.map PagesLocalesDetailMsg cmd
            )

        PagesLoginMsg pageMsg ->
            let
                updateConfig =
                    { wrapMsg = PagesLoginMsg
                    , setSessionMsg = SetSession
                    }

                ( login, cmd ) =
                    Login.update updateConfig model.appState pageMsg model.pages.login

                pages =
                    model.pages
            in
            ( { model | pages = { pages | login = login } }
            , cmd
            )

        PagesSignupMsg pageMsg ->
            let
                ( signup, cmd ) =
                    Signup.update model.appState pageMsg model.pages.signup

                pages =
                    model.pages
            in
            ( { model | pages = { pages | signup = signup } }
            , Cmd.map PagesSignupMsg cmd
            )

        PagesSignupConfirmationMsg pageMsg ->
            let
                signupConfirmation =
                    SignupConfirmation.update pageMsg model.appState model.pages.signupConfirmation

                pages =
                    model.pages
            in
            ( { model | pages = { pages | signupConfirmation = signupConfirmation } }
            , Cmd.none
            )

        PagesForgottenTokenMsg pageMsg ->
            let
                ( forgottenToken, cmd ) =
                    ForgottenToken.update model.appState pageMsg model.pages.forgottenToken

                pages =
                    model.pages
            in
            ( { model | pages = { pages | forgottenToken = forgottenToken } }
            , Cmd.map PagesForgottenTokenMsg cmd
            )

        PagesForgottenTokenConfirmationMsg pageMsg ->
            let
                forgottenTokenConfirmation =
                    ForgottenTokenConfirmation.update pageMsg model.appState model.pages.forgottenTokenConfirmation

                pages =
                    model.pages
            in
            ( { model | pages = { pages | forgottenTokenConfirmation = forgottenTokenConfirmation } }
            , Cmd.none
            )

        PagesOrganizationDetailMsg pageMsg ->
            let
                ( organizationDetail, cmd ) =
                    OragnizationDetail.update pageMsg model.appState model.pages.organizationDetail

                pages =
                    model.pages
            in
            ( { model | pages = { pages | organizationDetail = organizationDetail } }
            , Cmd.map PagesOrganizationDetailMsg cmd
            )

        AboutModalMsg aboutModalMsg ->
            let
                ( aboutModalModel, cmd ) =
                    AboutModal.update model.appState aboutModalMsg model.aboutModalModel
            in
            ( { model | aboutModalModel = aboutModalModel }
            , Cmd.map AboutModalMsg cmd
            )


initPage : Model -> ( Model, Cmd Msg )
initPage model =
    case model.appState.route of
        Routes.KnowledgeModels ->
            let
                ( knowledgeModels, cmd ) =
                    KnowledgeModels.init model.appState

                pages =
                    model.pages
            in
            ( { model | pages = { pages | knowledgeModels = knowledgeModels } }
            , Cmd.map PagesKnowledgeModelsMsg cmd
            )

        Routes.KnowledgeModelsDetail knowledgeModelId ->
            let
                ( knowledgeModelsDetail, cmd ) =
                    KnowledgeModelsDetail.init model.appState knowledgeModelId

                pages =
                    model.pages
            in
            ( { model | pages = { pages | knowledgeModelsDetail = knowledgeModelsDetail } }
            , Cmd.map PagesKnowledgeModelsDetailMsg cmd
            )

        Routes.DocumentTemplates ->
            let
                ( documentTemplates, cmd ) =
                    DocumentTemplates.init model.appState

                pages =
                    model.pages
            in
            ( { model | pages = { pages | documentTemplates = documentTemplates } }
            , Cmd.map PagesDocumentTemplatesMsg cmd
            )

        Routes.DocumentTemplatesDetail documentTemplateId ->
            let
                ( documentTemplatesDetail, cmd ) =
                    DocumentTemplatesDetail.init model.appState documentTemplateId

                pages =
                    model.pages
            in
            ( { model | pages = { pages | documentTemplatesDetail = documentTemplatesDetail } }
            , Cmd.map PagesDocumentTemplatesDetailMsg cmd
            )

        Routes.Locales ->
            let
                ( locales, cmd ) =
                    Locales.init model.appState

                pages =
                    model.pages
            in
            ( { model | pages = { pages | locales = locales } }
            , Cmd.map PagesLocalesMsg cmd
            )

        Routes.LocalesDetail localeId ->
            let
                ( localesDetail, cmd ) =
                    LocalesDetail.init model.appState localeId

                pages =
                    model.pages
            in
            ( { model | pages = { pages | localesDetail = localesDetail } }
            , Cmd.map PagesLocalesDetailMsg cmd
            )

        Routes.Login ->
            let
                pages =
                    model.pages
            in
            ( { model | pages = { pages | login = Login.initialModel } }
            , Cmd.none
            )

        Routes.Signup ->
            let
                pages =
                    model.pages
            in
            ( { model | pages = { pages | signup = Signup.initialModel model.appState } }
            , Cmd.none
            )

        Routes.SignupConfirmation organizationId hash ->
            let
                ( signupConfirmation, cmd ) =
                    SignupConfirmation.init model.appState organizationId hash

                pages =
                    model.pages
            in
            ( { model | pages = { pages | signupConfirmation = signupConfirmation } }
            , Cmd.map PagesSignupConfirmationMsg cmd
            )

        Routes.ForgottenToken ->
            let
                pages =
                    model.pages
            in
            ( { model | pages = { pages | forgottenToken = ForgottenToken.initialModel } }
            , Cmd.none
            )

        Routes.ForgottenTokenConfirmation orgId hash ->
            let
                ( forgottenTokenConfirmation, cmd ) =
                    ForgottenTokenConfirmation.init model.appState orgId hash

                pages =
                    model.pages
            in
            ( { model | pages = { pages | forgottenTokenConfirmation = forgottenTokenConfirmation } }
            , Cmd.map PagesForgottenTokenConfirmationMsg cmd
            )

        Routes.OrganizationDetail ->
            let
                ( organizationDetail, cmd ) =
                    OragnizationDetail.init model.appState

                pages =
                    model.pages
            in
            ( { model | pages = { pages | organizationDetail = organizationDetail } }
            , Cmd.map PagesOrganizationDetailMsg cmd
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    let
        content =
            case model.appState.route of
                Routes.Home ->
                    Homepage.view model.appState

                Routes.KnowledgeModels ->
                    Html.map PagesKnowledgeModelsMsg <|
                        KnowledgeModels.view model.appState model.pages.knowledgeModels

                Routes.KnowledgeModelsDetail _ ->
                    Html.map PagesKnowledgeModelsDetailMsg <|
                        KnowledgeModelsDetail.view model.appState model.pages.knowledgeModelsDetail

                Routes.DocumentTemplates ->
                    Html.map PagesDocumentTemplatesMsg <|
                        DocumentTemplates.view model.appState model.pages.documentTemplates

                Routes.DocumentTemplatesDetail _ ->
                    Html.map PagesDocumentTemplatesDetailMsg <|
                        DocumentTemplatesDetail.view model.appState model.pages.documentTemplatesDetail

                Routes.Locales ->
                    Html.map PagesLocalesMsg <|
                        Locales.view model.appState model.pages.locales

                Routes.LocalesDetail _ ->
                    Html.map PagesLocalesDetailMsg <|
                        LocalesDetail.view model.appState model.pages.localesDetail

                Routes.Login ->
                    Html.map PagesLoginMsg <|
                        Login.view model.appState model.pages.login

                Routes.Signup ->
                    Html.map PagesSignupMsg <|
                        Signup.view model.appState model.pages.signup

                Routes.SignupConfirmation _ _ ->
                    Html.map PagesSignupConfirmationMsg <|
                        SignupConfirmation.view model.appState model.pages.signupConfirmation

                Routes.ForgottenToken ->
                    Html.map PagesForgottenTokenMsg <|
                        ForgottenToken.view model.appState model.pages.forgottenToken

                Routes.ForgottenTokenConfirmation _ _ ->
                    Html.map PagesForgottenTokenConfirmationMsg <|
                        ForgottenTokenConfirmation.view model.appState model.pages.forgottenTokenConfirmation

                Routes.OrganizationDetail ->
                    Html.map PagesOrganizationDetailMsg <|
                        OragnizationDetail.view model.appState model.pages.organizationDetail

                Routes.NotFound ->
                    Page.illustratedMessage
                        { image = Undraw.pageNotFound
                        , heading = gettext "Not Found" model.appState.locale
                        , msg = gettext "The page you are looking for does not exist." model.appState.locale
                        }

                Routes.NotAllowed ->
                    Page.illustratedMessage
                        { image = Undraw.security
                        , heading = gettext "Not Allowed" model.appState.locale
                        , msg = gettext "You are not allowed to view this page." model.appState.locale
                        }
    in
    Layout.app model.appState
        { openAboutModalMsg = AboutModalMsg AboutModal.openMsg
        , logoutMsg = SetSession Nothing
        , openCloseMenuMsg = SetMenuVisible
        , content = content
        , aboutModal = Html.map AboutModalMsg <| AboutModal.view model.appState model.aboutModalModel
        , menuVisible = model.menuVisible
        }
