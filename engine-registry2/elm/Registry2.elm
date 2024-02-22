module Registry2 exposing (Model, Msg, main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, header, i, img, li, main_, p, section, small, strong, text, ul)
import Html.Attributes exposing (class, classList, height, href, src)
import Html.Events exposing (onClick)
import Json.Decode as D
import Maybe.Extra as Maybe
import Registry2.Components.FontAwesome exposing (fas)
import Registry2.Data.AppState as AppState exposing (AppState)
import Registry2.Data.Session as Session exposing (Session)
import Registry2.Pages.DocumentTemplates as DocumentTemplates
import Registry2.Pages.DocumentTemplatesDetail as DocumentTemplatesDetail
import Registry2.Pages.KnowledgeModels as KnowledgeModels
import Registry2.Pages.KnowledgeModelsDetail as KnowledgeModelsDetail
import Registry2.Pages.Locales as Locales
import Registry2.Pages.LocalesDetail as LocalesDetail
import Registry2.Pages.Login as Login
import Registry2.Pages.Signup as Signup
import Registry2.Ports as Ports
import Registry2.Routes as Routes
import Shared.Utils exposing (dispatch)
import Task
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
        }
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
                    let
                        originalRoute =
                            Routes.parse url
                    in
                    ( { appStateWithoutRoute | route = routeIfAllowed originalRoute }
                    , dispatch (OnUrlChange url)
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
                , signup = Signup.initialModel
                }
            }
    in
    ( model, Cmd.batch [ cmd, Task.perform OnTimeZone Time.here ] )


routeIfAllowed : Routes.Route -> Routes.Route
routeIfAllowed route =
    if Routes.isAllowed route then
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange url ->
            let
                nextRoute =
                    routeIfAllowed (Routes.parse url)
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
                    case Debug.log "set session" session of
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
            ( { model | pages = { pages | signup = Signup.initialModel } }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    let
        content =
            case model.appState.route of
                Routes.Home ->
                    text "Homepage"

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

                Routes.ForgottenToken ->
                    text "Forgotten Token"

                Routes.OrganizationDetail ->
                    text "Organization Detail"

                Routes.NotFound ->
                    text "Not Found"

                Routes.NotAllowed ->
                    text "Not Allowed"
    in
    { title = "DSW Registry"
    , body =
        -- https://themes.getbootstrap.com/preview/?theme_id=35287
        [ main_ []
            [ header [ class "shadow-sm navbar-fixed" ]
                [ div [ class "navbar-sticky bg-white" ]
                    [ div [ class "navbar navbar-expand-lg navbar-light" ]
                        [ div [ class "container" ]
                            [ a
                                [ class "navbar-brand"
                                , href (Routes.toUrl Routes.home)
                                ]
                                [ div [ class "d-flex" ]
                                    [ img [ src "/img/logo.svg", height 30, class "me-1" ] []
                                    , text "DSW Registry"
                                    ]
                                ]
                            , div [ class "navbar-toolbar d-flex flex-shrink-0 align-items-center" ]
                                ([ a [ class "navbar-tool" ]
                                    [ div [ class "navbar-tool-icon-box" ]
                                        [ fas "fa-lg fa-info-circle" ]
                                    ]
                                 ]
                                    ++ profileNavigation model.appState
                                    ++ [ a [ class "navbar-tool d-lg-none d-sm-flex", onClick (SetMenuVisible (not model.menuVisible)) ]
                                            [ div [ class "navbar-tool-icon-box" ]
                                                [ fas "fa-lg fa-bars" ]
                                            ]
                                       ]
                                )
                            ]
                        ]
                    , div [ class "navbar navbar-expand-lg navbar-light" ]
                        [ div [ class "container" ]
                            [ div [ class "navbar-collapse collapse", classList [ ( "show", model.menuVisible ) ] ]
                                [ ul [ class "navbar-nav" ]
                                    [ li [ class "nav-item" ]
                                        [ a
                                            [ class "nav-link"
                                            , classList [ ( "active", model.appState.route == Routes.Home ) ]
                                            , href (Routes.toUrl Routes.home)
                                            ]
                                            [ fas "fa-home"
                                            , text (gettext "Home" model.appState.locale)
                                            ]
                                        ]
                                    , li [ class "nav-item" ]
                                        [ a
                                            [ class "nav-link"
                                            , classList [ ( "active", model.appState.route == Routes.KnowledgeModels ) ]
                                            , href (Routes.toUrl Routes.knowledgeModels)
                                            ]
                                            [ fas "fa-sitemap"
                                            , text (gettext "Knowledge Models" model.appState.locale)
                                            ]
                                        ]
                                    , li [ class "nav-item" ]
                                        [ a
                                            [ class "nav-link"
                                            , classList [ ( "active", model.appState.route == Routes.DocumentTemplates ) ]
                                            , href (Routes.toUrl Routes.documentTemplates)
                                            ]
                                            [ fas "fa-file-code"
                                            , text (gettext "Document Templates" model.appState.locale)
                                            ]
                                        ]
                                    , li [ class "nav-item" ]
                                        [ a
                                            [ class "nav-link"
                                            , classList [ ( "active", model.appState.route == Routes.Locales ) ]
                                            , href (Routes.toUrl Routes.locales)
                                            ]
                                            [ fas "fa-language"
                                            , text (gettext "Locales" model.appState.locale)
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , section [ class "container pt-5" ]
                [ content ]
            ]
        ]
    }


profileNavigation : AppState -> List (Html Msg)
profileNavigation appState =
    case appState.session of
        Just session ->
            [ a
                [ class "navbar-tool d-sm-flex d-lg-none"
                , href (Routes.toUrl Routes.organizationDetail)
                ]
                [ div [ class "navbar-tool-icon-box" ]
                    [ fas "fa-lg fa-user" ]
                ]
            , a
                [ class "navbar-tool d-sm-flex d-lg-none"
                , onClick (SetSession Nothing)
                ]
                [ div [ class "navbar-tool-icon-box" ]
                    [ fas "fa-lg fa-sign-out-alt" ]
                ]
            , div [ class "navbar-profile d-lg-flex d-sm-none" ]
                [ div [ class "navbar-tool-icon-box" ]
                    [ fas "fa-lg fa-user" ]
                , div [ class "d-flex flex-column justify-content-center" ]
                    [ small [ class "organization-name" ] [ text session.organizationName ]
                    , div [ class "text-muted" ]
                        [ a [ href (Routes.toUrl Routes.organizationDetail) ] [ text "Edit" ]
                        , text " • "
                        , a [ onClick (SetSession Nothing) ] [ text "Logout" ]
                        ]
                    ]
                ]
            ]

        Nothing ->
            [ a
                [ class "navbar-tool"
                , href (Routes.toUrl Routes.login)
                ]
                [ div [ class "navbar-tool-icon-box" ]
                    [ fas "fa-lg fa-user" ]
                ]
            ]
