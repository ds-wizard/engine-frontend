module Wizard.Update exposing (update)

import Browser
import Browser.Navigation exposing (load, pushUrl)
import Shared.Auth.Session as Session
import Url
import Wizard.Apps.Update
import Wizard.Auth.Update
import Wizard.Common.AppState as AppState
import Wizard.Common.Menu.Update
import Wizard.Common.Time as Time
import Wizard.Dashboard.Update
import Wizard.Dev.Update
import Wizard.Documents.Update
import Wizard.KMEditor.Update
import Wizard.KnowledgeModels.Update
import Wizard.Models exposing (Model, initLocalModel, setRoute, setSeed, setSession)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Ports as Ports
import Wizard.Projects.Update
import Wizard.Public.Update
import Wizard.Registry.Update
import Wizard.Routes as Routes
import Wizard.Routing exposing (parseLocation)
import Wizard.Settings.Update
import Wizard.Templates.Update
import Wizard.Users.Update


fetchData : Model -> Cmd Msg
fetchData model =
    case model.appState.route of
        Routes.DevRoute route ->
            Cmd.map AdminMsg <|
                Wizard.Dev.Update.fetchData route model.appState

        Routes.AppsRoute route ->
            Cmd.map AppsMsg <|
                Wizard.Apps.Update.fetchData route model.appState

        Routes.DashboardRoute ->
            Cmd.map DashboardMsg <|
                Wizard.Dashboard.Update.fetchData model.appState

        Routes.DocumentsRoute _ ->
            Cmd.map DocumentsMsg <|
                Wizard.Documents.Update.fetchData model.appState model.documentsModel

        Routes.KMEditorRoute route ->
            Cmd.map Wizard.Msgs.KMEditorMsg <|
                Wizard.KMEditor.Update.fetchData route model.kmEditorModel model.appState

        Routes.KnowledgeModelsRoute route ->
            Cmd.map Wizard.Msgs.KnowledgeModelsMsg <|
                Wizard.KnowledgeModels.Update.fetchData route model.appState

        Routes.ProjectsRoute route ->
            Cmd.map Wizard.Msgs.ProjectsMsg <|
                Wizard.Projects.Update.fetchData route model.appState model.projectsModel

        Routes.PublicRoute route ->
            Cmd.map Wizard.Msgs.PublicMsg <|
                Wizard.Public.Update.fetchData route model.appState

        Routes.RegistryRoute route ->
            Cmd.map Wizard.Msgs.RegistryMsg <|
                Wizard.Registry.Update.fetchData route model.appState

        Routes.SettingsRoute route ->
            Cmd.map Wizard.Msgs.SettingsMsg <|
                Wizard.Settings.Update.fetchData route model.appState model.settingsModel

        Routes.TemplatesRoute route ->
            Cmd.map Wizard.Msgs.TemplatesMsg <|
                Wizard.Templates.Update.fetchData route model.appState

        Routes.UsersRoute route ->
            Cmd.map Wizard.Msgs.UsersMsg <|
                Wizard.Users.Update.fetchData route model.appState

        _ ->
            Cmd.none


isGuarded : Model -> Maybe String
isGuarded model =
    case model.appState.route of
        Routes.KMEditorRoute route ->
            Wizard.KMEditor.Update.isGuarded route model.appState model.kmEditorModel

        Routes.ProjectsRoute route ->
            Wizard.Projects.Update.isGuarded route model.appState model.projectsModel

        _ ->
            Nothing


onUnload : Routes.Route -> Model -> Cmd Msg
onUnload newRoute model =
    case model.appState.route of
        Routes.KMEditorRoute route ->
            Cmd.map KMEditorMsg <|
                Wizard.KMEditor.Update.onUnload route newRoute model.kmEditorModel

        Routes.ProjectsRoute route ->
            Cmd.map ProjectsMsg <|
                Wizard.Projects.Update.onUnload route newRoute model.projectsModel

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        wrapGetTime ( m, cmd ) =
            case msg of
                Wizard.Msgs.OnTime _ ->
                    ( m, cmd )

                _ ->
                    ( m, Cmd.batch [ Time.getTime, cmd ] )
    in
    wrapGetTime <|
        case msg of
            Wizard.Msgs.OnUrlChange location ->
                let
                    newRoute =
                        parseLocation model.appState location

                    newModel =
                        setRoute newRoute model
                            |> initLocalModel
                in
                ( newModel, Cmd.batch [ onUnload newRoute model, fetchData newModel ] )

            Wizard.Msgs.OnUrlRequest urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        case isGuarded model of
                            Just guardMsg ->
                                ( model, Ports.alert guardMsg )

                            Nothing ->
                                ( model, pushUrl model.appState.key (Url.toString url) )

                    Browser.External url ->
                        if url == "" then
                            ( model, Cmd.none )

                        else
                            ( model, load url )

            Wizard.Msgs.OnTime time ->
                ( { model | appState = AppState.setCurrentTime model.appState time }, Cmd.none )

            Wizard.Msgs.OnTimeZone timeZone ->
                ( { model | appState = AppState.setTimeZone model.appState timeZone }, Cmd.none )

            Wizard.Msgs.AcceptCookies ->
                ( { model | appState = AppState.acceptCookies model.appState }, Ports.acceptCookies () )

            Wizard.Msgs.AuthMsg authMsg ->
                Wizard.Auth.Update.update authMsg model

            Wizard.Msgs.SetSidebarCollapsed collapsed ->
                let
                    newSession =
                        Session.setSidebarCollapsed model.appState.session collapsed

                    newModel =
                        setSession newSession model
                in
                ( newModel, Ports.storeSession <| Session.encode newSession )

            Wizard.Msgs.SetFullscreen fullscreen ->
                let
                    newSession =
                        Session.setFullscreen model.appState.session fullscreen

                    newModel =
                        setSession newSession model
                in
                ( newModel, Ports.storeSession <| Session.encode newSession )

            Wizard.Msgs.MenuMsg menuMsg ->
                let
                    ( menuModel, cmd ) =
                        Wizard.Common.Menu.Update.update Wizard.Msgs.MenuMsg menuMsg model.appState model.menuModel
                in
                ( { model | menuModel = menuModel }, cmd )

            Wizard.Msgs.AdminMsg adminMsg ->
                let
                    ( adminModel, cmd ) =
                        Wizard.Dev.Update.update adminMsg Wizard.Msgs.AdminMsg model.appState model.adminModel
                in
                ( { model | adminModel = adminModel }, cmd )

            Wizard.Msgs.AppsMsg appsMsg ->
                let
                    ( appsModel, cmd ) =
                        Wizard.Apps.Update.update appsMsg Wizard.Msgs.AppsMsg model.appState model.appsModel
                in
                ( { model | appsModel = appsModel }, cmd )

            Wizard.Msgs.DashboardMsg dashboardMsg ->
                let
                    ( dashboardModel, cmd ) =
                        Wizard.Dashboard.Update.update dashboardMsg model.appState model.dashboardModel
                in
                ( { model | dashboardModel = dashboardModel }, cmd )

            Wizard.Msgs.DocumentsMsg documentsMsg ->
                let
                    ( documentsModel, cmd ) =
                        Wizard.Documents.Update.update Wizard.Msgs.DocumentsMsg documentsMsg model.appState model.documentsModel
                in
                ( { model | documentsModel = documentsModel }, cmd )

            Wizard.Msgs.KMEditorMsg kmEditorMsg ->
                let
                    ( seed, kmEditorModel, cmd ) =
                        Wizard.KMEditor.Update.update kmEditorMsg Wizard.Msgs.KMEditorMsg model.appState model.kmEditorModel
                in
                ( setSeed seed { model | kmEditorModel = kmEditorModel }, cmd )

            Wizard.Msgs.KnowledgeModelsMsg kmPackagesMsg ->
                let
                    ( seed, kmPackagesModel, cmd ) =
                        Wizard.KnowledgeModels.Update.update kmPackagesMsg Wizard.Msgs.KnowledgeModelsMsg model.appState model.kmPackagesModel
                in
                ( setSeed seed { model | kmPackagesModel = kmPackagesModel }, cmd )

            Wizard.Msgs.ProjectsMsg plansMsg ->
                let
                    ( seed, plansModel, cmd ) =
                        Wizard.Projects.Update.update Wizard.Msgs.ProjectsMsg plansMsg model.appState model.projectsModel
                in
                ( setSeed seed { model | projectsModel = plansModel }, cmd )

            Wizard.Msgs.PublicMsg publicMsg ->
                let
                    ( publicModel, cmd ) =
                        Wizard.Public.Update.update publicMsg Wizard.Msgs.PublicMsg model.appState model.publicModel
                in
                ( { model | publicModel = publicModel }, cmd )

            Wizard.Msgs.RegistryMsg registryMsg ->
                let
                    ( registryModel, cmd ) =
                        Wizard.Registry.Update.update registryMsg Wizard.Msgs.RegistryMsg model.appState model.registryModel
                in
                ( { model | registryModel = registryModel }, cmd )

            Wizard.Msgs.SettingsMsg settingsMsg ->
                let
                    ( settingsModel, cmd ) =
                        Wizard.Settings.Update.update Wizard.Msgs.SettingsMsg settingsMsg model.appState model.settingsModel
                in
                ( { model | settingsModel = settingsModel }, cmd )

            Wizard.Msgs.TemplatesMsg templatesMsg ->
                let
                    ( templatesModel, cmd ) =
                        Wizard.Templates.Update.update templatesMsg Wizard.Msgs.TemplatesMsg model.appState model.templatesModel
                in
                ( { model | templatesModel = templatesModel }, cmd )

            Wizard.Msgs.UsersMsg usersMsg ->
                let
                    ( seed, users, cmd ) =
                        Wizard.Users.Update.update usersMsg Wizard.Msgs.UsersMsg model.appState model.users
                in
                ( setSeed seed { model | users = users }, cmd )
