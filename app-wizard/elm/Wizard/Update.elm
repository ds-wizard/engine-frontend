module Wizard.Update exposing (update)

import Browser
import Browser.Navigation as Navigation exposing (load, pushUrl)
import Common.Components.AIAssistant as AIAssistant
import Common.Ports.Window as Window
import Common.Utils.TimeUtils as TimeUtils
import Url
import Wizard.Api.Tours as ToursApi
import Wizard.Components.Menu.Update
import Wizard.Data.AppState as AppState
import Wizard.Data.Session as Session
import Wizard.Models exposing (Model, addTour, initLocalModel, setRoute, setSeed, setSession)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Pages.Auth.Update
import Wizard.Pages.Comments.Update
import Wizard.Pages.Dashboard.Update
import Wizard.Pages.Dev.Update
import Wizard.Pages.DocumentTemplateEditors.Update
import Wizard.Pages.DocumentTemplates.Update
import Wizard.Pages.Documents.Update
import Wizard.Pages.KMEditor.Update
import Wizard.Pages.KnowledgeModelSecrets.Update
import Wizard.Pages.KnowledgeModels.Update
import Wizard.Pages.Locales.Update
import Wizard.Pages.ProjectActions.Update
import Wizard.Pages.ProjectFiles.Update
import Wizard.Pages.ProjectImporters.Update
import Wizard.Pages.Projects.Update
import Wizard.Pages.Public.Update
import Wizard.Pages.Registry.Update
import Wizard.Pages.Settings.Update
import Wizard.Pages.Tenants.Update
import Wizard.Pages.Users.Update
import Wizard.Ports.Cookies as Cookies
import Wizard.Ports.Session as Session
import Wizard.Routes as Routes
import Wizard.Routing exposing (parseLocation, routeIfAllowed)


fetchData : Model -> Cmd Msg
fetchData model =
    case model.appState.route of
        Routes.DevRoute route ->
            Cmd.map AdminMsg <|
                Wizard.Pages.Dev.Update.fetchData route model.appState

        Routes.TenantsRoute route ->
            Cmd.map TenantsMsg <|
                Wizard.Pages.Tenants.Update.fetchData route model.appState

        Routes.CommentsRoute _ _ ->
            Cmd.map Wizard.Msgs.CommentsMsg <|
                Wizard.Pages.Comments.Update.fetchData

        Routes.DashboardRoute ->
            Cmd.map DashboardMsg <|
                Wizard.Pages.Dashboard.Update.fetchData model.appState model.dashboardModel

        Routes.DocumentsRoute _ ->
            Cmd.map DocumentsMsg <|
                Wizard.Pages.Documents.Update.fetchData model.appState model.documentsModel

        Routes.DocumentTemplateEditorsRoute route ->
            Cmd.map Wizard.Msgs.DocumentTemplateEditorsMsg <|
                Wizard.Pages.DocumentTemplateEditors.Update.fetchData route model.appState model.documentTemplateEditorsModel

        Routes.DocumentTemplatesRoute route ->
            Cmd.map Wizard.Msgs.DocumentTemplatesMsg <|
                Wizard.Pages.DocumentTemplates.Update.fetchData route model.appState

        Routes.KMEditorRoute route ->
            Cmd.map Wizard.Msgs.KMEditorMsg <|
                Wizard.Pages.KMEditor.Update.fetchData route model.kmEditorModel model.appState

        Routes.KnowledgeModelsRoute route ->
            Cmd.map Wizard.Msgs.KnowledgeModelsMsg <|
                Wizard.Pages.KnowledgeModels.Update.fetchData route model.appState

        Routes.KnowledgeModelSecretsRoute ->
            Cmd.map Wizard.Msgs.KnowledgeModelSecretsMsg <|
                Wizard.Pages.KnowledgeModelSecrets.Update.fetchData model.appState

        Routes.LocalesRoute route ->
            Cmd.map Wizard.Msgs.LocaleMsg <|
                Wizard.Pages.Locales.Update.fetchData route model.appState

        Routes.ProjectActionsRoute _ ->
            Cmd.map Wizard.Msgs.ProjectActionsMsg <|
                Wizard.Pages.ProjectActions.Update.fetchData

        Routes.ProjectFilesRoute _ ->
            Cmd.map Wizard.Msgs.ProjectFilesMsg <|
                Wizard.Pages.ProjectFiles.Update.fetchData

        Routes.ProjectImportersRoute _ ->
            Cmd.map Wizard.Msgs.ProjectImportersMsg <|
                Wizard.Pages.ProjectImporters.Update.fetchData

        Routes.ProjectsRoute route ->
            Cmd.map Wizard.Msgs.ProjectsMsg <|
                Wizard.Pages.Projects.Update.fetchData route model.appState model.projectsModel

        Routes.PublicRoute route ->
            Cmd.map Wizard.Msgs.PublicMsg <|
                Wizard.Pages.Public.Update.fetchData route model.appState

        Routes.RegistryRoute route ->
            Cmd.map Wizard.Msgs.RegistryMsg <|
                Wizard.Pages.Registry.Update.fetchData route model.appState

        Routes.SettingsRoute route ->
            Cmd.map Wizard.Msgs.SettingsMsg <|
                Wizard.Pages.Settings.Update.fetchData route model.appState model.settingsModel

        Routes.UsersRoute route ->
            Cmd.map Wizard.Msgs.UsersMsg <|
                Wizard.Pages.Users.Update.fetchData route model.appState

        _ ->
            Cmd.none


isGuarded : Routes.Route -> Model -> Maybe String
isGuarded nextRoute model =
    case model.appState.route of
        Routes.DocumentTemplateEditorsRoute route ->
            Wizard.Pages.DocumentTemplateEditors.Update.isGuarded route model.appState nextRoute model.documentTemplateEditorsModel

        Routes.KMEditorRoute route ->
            Wizard.Pages.KMEditor.Update.isGuarded route model.appState nextRoute model.kmEditorModel

        Routes.ProjectsRoute route ->
            Wizard.Pages.Projects.Update.isGuarded route model.appState nextRoute model.projectsModel

        _ ->
            Nothing


onUnload : Routes.Route -> Model -> Cmd Msg
onUnload nextRoute model =
    case model.appState.route of
        Routes.KMEditorRoute route ->
            Cmd.map KMEditorMsg <|
                Wizard.Pages.KMEditor.Update.onUnload route nextRoute model.kmEditorModel

        Routes.ProjectsRoute route ->
            Cmd.map ProjectsMsg <|
                Wizard.Pages.Projects.Update.onUnload route nextRoute model.projectsModel

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
                    ( m, Cmd.batch [ TimeUtils.getTime OnTime, cmd ] )
    in
    wrapGetTime <|
        case msg of
            Wizard.Msgs.OnUrlChange location ->
                let
                    nextRoute =
                        routeIfAllowed model.appState <|
                            parseLocation model.appState location

                    modelWithRoute =
                        setRoute nextRoute model

                    originalRoute =
                        model.appState.route
                in
                if Routes.isSameListingRoute originalRoute nextRoute then
                    ( modelWithRoute, Cmd.none )

                else
                    let
                        newModel =
                            initLocalModel model.appState modelWithRoute
                    in
                    ( newModel, Cmd.batch [ onUnload nextRoute model, fetchData newModel ] )

            Wizard.Msgs.OnUrlRequest urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        let
                            isWizardRoute =
                                String.startsWith "/wizard" url.path
                        in
                        if isWizardRoute then
                            let
                                nextRoute =
                                    parseLocation model.appState url
                            in
                            case isGuarded nextRoute model of
                                Just guardMsg ->
                                    ( model, Window.alert guardMsg )

                                Nothing ->
                                    ( model, pushUrl model.appState.key (Url.toString url) )

                        else
                            ( model, load (Url.toString url) )

                    Browser.External url ->
                        if url == "" then
                            ( model, Cmd.none )

                        else
                            ( model, load url )

            Wizard.Msgs.HistoryBackCallback url ->
                if String.isEmpty url then
                    ( model, Navigation.back model.appState.key 1 )

                else
                    ( model, Navigation.pushUrl model.appState.key url )

            Wizard.Msgs.OnTime time ->
                ( { model | appState = AppState.setCurrentTime model.appState time }, Cmd.none )

            Wizard.Msgs.OnTimeZone timeZone ->
                ( { model | appState = AppState.setTimeZone model.appState timeZone }, Cmd.none )

            Wizard.Msgs.AcceptCookies ->
                ( { model | appState = AppState.acceptCookies model.appState }, Cookies.acceptCookies () )

            Wizard.Msgs.AuthMsg authMsg ->
                Wizard.Pages.Auth.Update.update authMsg model

            Wizard.Msgs.AIAssistantMsg aiAssistantMsg ->
                let
                    updateConfig =
                        { serverInfo = AppState.toAIAssistantServerInfo model.appState
                        , seed = model.appState.seed
                        }

                    ( newSeed, aiAssistantState, aiAssistantCmd ) =
                        AIAssistant.update updateConfig aiAssistantMsg model.aiAssistantState
                in
                ( setSeed newSeed <| { model | aiAssistantState = aiAssistantState }
                , Cmd.map AIAssistantMsg aiAssistantCmd
                )

            Wizard.Msgs.SetSidebarCollapsed collapsed ->
                let
                    newSession =
                        Session.setSidebarCollapsed model.appState.session collapsed

                    newModel =
                        setSession newSession model
                in
                ( newModel, Session.storeSession <| Session.encode newSession )

            Wizard.Msgs.SetRightPanelCollapsed collapsed ->
                let
                    newSession =
                        Session.setRightPanelCollapsed model.appState.session collapsed

                    newModel =
                        setSession newSession model
                in
                ( newModel, Session.storeSession <| Session.encode newSession )

            Wizard.Msgs.SetFullscreen fullscreen ->
                let
                    newSession =
                        Session.setFullscreen model.appState.session fullscreen

                    newModel =
                        setSession newSession model
                in
                ( newModel, Session.storeSession <| Session.encode newSession )

            Wizard.Msgs.HideSessionExpiresSoonModal ->
                let
                    appState =
                        model.appState
                in
                ( { model | appState = { appState | sessionExpiresSoonModalHidden = True } }
                , Cmd.none
                )

            Wizard.Msgs.MenuMsg menuMsg ->
                let
                    ( menuModel, cmd ) =
                        Wizard.Components.Menu.Update.update Wizard.Msgs.MenuMsg menuMsg model.appState model.menuModel
                in
                ( { model | menuModel = menuModel }, cmd )

            Wizard.Msgs.AdminMsg adminMsg ->
                let
                    ( adminModel, cmd ) =
                        Wizard.Pages.Dev.Update.update adminMsg Wizard.Msgs.AdminMsg model.appState model.adminModel
                in
                ( { model | adminModel = adminModel }, cmd )

            Wizard.Msgs.CommentsMsg commentsMsg ->
                let
                    ( commentsModel, cmd ) =
                        Wizard.Pages.Comments.Update.update Wizard.Msgs.CommentsMsg commentsMsg model.appState model.commentsModel
                in
                ( { model | commentsModel = commentsModel }, cmd )

            Wizard.Msgs.DashboardMsg dashboardMsg ->
                let
                    ( dashboardModel, cmd ) =
                        Wizard.Pages.Dashboard.Update.update dashboardMsg model.appState model.dashboardModel
                in
                ( { model | dashboardModel = dashboardModel }, cmd )

            Wizard.Msgs.DocumentsMsg documentsMsg ->
                let
                    ( documentsModel, cmd ) =
                        Wizard.Pages.Documents.Update.update Wizard.Msgs.DocumentsMsg documentsMsg model.appState model.documentsModel
                in
                ( { model | documentsModel = documentsModel }, cmd )

            Wizard.Msgs.DocumentTemplateEditorsMsg templatesMsg ->
                let
                    ( seed, templatesModel, cmd ) =
                        Wizard.Pages.DocumentTemplateEditors.Update.update templatesMsg Wizard.Msgs.DocumentTemplateEditorsMsg model.appState model.documentTemplateEditorsModel
                in
                ( setSeed seed { model | documentTemplateEditorsModel = templatesModel }, cmd )

            Wizard.Msgs.DocumentTemplatesMsg templatesMsg ->
                let
                    ( seed, templatesModel, cmd ) =
                        Wizard.Pages.DocumentTemplates.Update.update templatesMsg Wizard.Msgs.DocumentTemplatesMsg model.appState model.documentTemplatesModel
                in
                ( setSeed seed { model | documentTemplatesModel = templatesModel }, cmd )

            Wizard.Msgs.KMEditorMsg kmEditorMsg ->
                let
                    ( seed, kmEditorModel, cmd ) =
                        Wizard.Pages.KMEditor.Update.update kmEditorMsg Wizard.Msgs.KMEditorMsg model.appState model.kmEditorModel
                in
                ( setSeed seed { model | kmEditorModel = kmEditorModel }, cmd )

            Wizard.Msgs.KnowledgeModelsMsg kmPackagesMsg ->
                let
                    ( seed, kmPackagesModel, cmd ) =
                        Wizard.Pages.KnowledgeModels.Update.update kmPackagesMsg Wizard.Msgs.KnowledgeModelsMsg model.appState model.kmPackagesModel
                in
                ( setSeed seed { model | kmPackagesModel = kmPackagesModel }, cmd )

            Wizard.Msgs.KnowledgeModelSecretsMsg secretsMsg ->
                let
                    updateConfig =
                        { wrapMsg = Wizard.Msgs.KnowledgeModelSecretsMsg
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        }

                    ( secretsModel, cmd ) =
                        Wizard.Pages.KnowledgeModelSecrets.Update.update updateConfig model.appState secretsMsg model.kmSecretsModel
                in
                ( { model | kmSecretsModel = secretsModel }, cmd )

            Wizard.Msgs.LocaleMsg localeMsg ->
                let
                    ( localeModel, cmd ) =
                        Wizard.Pages.Locales.Update.update localeMsg Wizard.Msgs.LocaleMsg model.appState model.localeModel
                in
                ( { model | localeModel = localeModel }, cmd )

            Wizard.Msgs.ProjectActionsMsg projectActionsMsg ->
                let
                    ( projectActionsModel, cmd ) =
                        Wizard.Pages.ProjectActions.Update.update projectActionsMsg Wizard.Msgs.ProjectActionsMsg model.appState model.projectActionsModel
                in
                ( { model | projectActionsModel = projectActionsModel }, cmd )

            Wizard.Msgs.ProjectFilesMsg projectFilesMsg ->
                let
                    ( projectFilesModel, cmd ) =
                        Wizard.Pages.ProjectFiles.Update.update projectFilesMsg Wizard.Msgs.ProjectFilesMsg model.appState model.projectFilesModel
                in
                ( { model | projectFilesModel = projectFilesModel }, cmd )

            Wizard.Msgs.ProjectImportersMsg projectImporterMsg ->
                let
                    ( projectImportersModel, cmd ) =
                        Wizard.Pages.ProjectImporters.Update.update projectImporterMsg Wizard.Msgs.ProjectImportersMsg model.appState model.projectImportersModel
                in
                ( { model | projectImportersModel = projectImportersModel }, cmd )

            Wizard.Msgs.ProjectsMsg plansMsg ->
                let
                    ( seed, plansModel, cmd ) =
                        Wizard.Pages.Projects.Update.update Wizard.Msgs.ProjectsMsg plansMsg model.appState model.projectsModel
                in
                ( setSeed seed { model | projectsModel = plansModel }, cmd )

            Wizard.Msgs.PublicMsg publicMsg ->
                let
                    ( publicModel, cmd ) =
                        Wizard.Pages.Public.Update.update publicMsg Wizard.Msgs.PublicMsg model.appState model.publicModel
                in
                ( { model | publicModel = publicModel }, cmd )

            Wizard.Msgs.RegistryMsg registryMsg ->
                let
                    ( registryModel, cmd ) =
                        Wizard.Pages.Registry.Update.update registryMsg Wizard.Msgs.RegistryMsg model.appState model.registryModel
                in
                ( { model | registryModel = registryModel }, cmd )

            Wizard.Msgs.SettingsMsg settingsMsg ->
                let
                    ( settingsModel, cmd ) =
                        Wizard.Pages.Settings.Update.update Wizard.Msgs.SettingsMsg settingsMsg model.appState model.settingsModel
                in
                ( { model | settingsModel = settingsModel }, cmd )

            Wizard.Msgs.TenantsMsg tenantsMsg ->
                let
                    ( tenantsModel, cmd ) =
                        Wizard.Pages.Tenants.Update.update tenantsMsg Wizard.Msgs.TenantsMsg model.appState model.tenantsModel
                in
                ( { model | tenantsModel = tenantsModel }, cmd )

            Wizard.Msgs.UsersMsg usersMsg ->
                let
                    ( seed, users, cmd ) =
                        Wizard.Pages.Users.Update.update usersMsg Wizard.Msgs.UsersMsg model.appState model.users
                in
                ( setSeed seed { model | users = users }, cmd )

            Wizard.Msgs.TourDone tourId ->
                ( addTour tourId model
                , ToursApi.putTour model.appState tourId (always Wizard.Msgs.TourPutCompleted)
                )

            Wizard.Msgs.TourPutCompleted ->
                ( model, Cmd.none )
