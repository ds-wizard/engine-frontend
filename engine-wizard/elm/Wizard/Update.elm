module Wizard.Update exposing (fetchData, update)

import Browser
import Browser.Navigation exposing (load, pushUrl)
import Shared.Auth.Session as Session
import Url exposing (Url)
import Wizard.Auth.Update
import Wizard.Common.AppState as AppState
import Wizard.Common.Menu.Update
import Wizard.Common.Time as Time
import Wizard.Dashboard.Update
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
    let
        fetchCmd =
            case model.appState.route of
                Routes.DashboardRoute ->
                    Cmd.map DashboardMsg <|
                        Wizard.Dashboard.Update.fetchData model.appState

                Routes.DocumentsRoute route ->
                    Cmd.map DocumentsMsg <|
                        Wizard.Documents.Update.fetchData route model.appState model.documentsModel

                Routes.KMEditorRoute route ->
                    Cmd.map Wizard.Msgs.KMEditorMsg <|
                        Wizard.KMEditor.Update.fetchData route model.kmEditorModel model.appState

                Routes.KnowledgeModelsRoute route ->
                    Cmd.map Wizard.Msgs.KnowledgeModelsMsg <|
                        Wizard.KnowledgeModels.Update.fetchData route model.appState

                Routes.ProjectsRoute route ->
                    Cmd.map Wizard.Msgs.PlansMsg <|
                        Wizard.Projects.Update.fetchData route model.appState model.plansModel

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
    in
    Cmd.batch [ fetchCmd, Time.getTime ]


isGuarded : Model -> Maybe String
isGuarded model =
    case model.appState.route of
        Routes.KMEditorRoute route ->
            Wizard.KMEditor.Update.isGuarded route model.appState model.kmEditorModel

        _ ->
            Nothing


onUnload : Routes.Route -> Model -> Cmd Msg
onUnload newRoute model =
    case model.appState.route of
        Routes.ProjectsRoute route ->
            Wizard.Projects.Update.onUnload route newRoute model.plansModel

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

        Wizard.Msgs.MenuMsg menuMsg ->
            let
                ( menuModel, cmd ) =
                    Wizard.Common.Menu.Update.update Wizard.Msgs.MenuMsg menuMsg model.appState model.menuModel
            in
            ( { model | menuModel = menuModel }, cmd )

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
                ( kmPackagesModel, cmd ) =
                    Wizard.KnowledgeModels.Update.update kmPackagesMsg Wizard.Msgs.KnowledgeModelsMsg model.appState model.kmPackagesModel
            in
            ( { model | kmPackagesModel = kmPackagesModel }, cmd )

        Wizard.Msgs.PlansMsg plansMsg ->
            let
                ( seed, plansModel, cmd ) =
                    Wizard.Projects.Update.update Wizard.Msgs.PlansMsg plansMsg model.appState model.plansModel
            in
            ( setSeed seed { model | plansModel = plansModel }, cmd )

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
