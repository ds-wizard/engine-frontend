module Wizard.Update exposing (fetchData, update)

import Browser
import Browser.Navigation exposing (load, pushUrl)
import Url exposing (Url)
import Wizard.Auth.Update
import Wizard.Common.AppState as AppState
import Wizard.Common.Menu.Update
import Wizard.Common.Session as Session
import Wizard.Common.Time as Time
import Wizard.Dashboard.Update
import Wizard.KMEditor.Update
import Wizard.KnowledgeModels.Update
import Wizard.Models exposing (Model, initLocalModel, setRoute, setSeed, setSession)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Organization.Update
import Wizard.Ports as Ports
import Wizard.Public.Update
import Wizard.Questionnaires.Update
import Wizard.Routes as Routes
import Wizard.Routing exposing (parseLocation)
import Wizard.Users.Update


fetchData : Model -> Cmd Msg
fetchData model =
    let
        fetchCmd =
            case model.appState.route of
                Routes.DashboardRoute ->
                    Cmd.map DashboardMsg <|
                        Wizard.Dashboard.Update.fetchData model.appState

                Routes.QuestionnairesRoute route ->
                    Cmd.map Wizard.Msgs.QuestionnairesMsg <|
                        Wizard.Questionnaires.Update.fetchData route model.appState model.questionnairesModel

                Routes.KMEditorRoute route ->
                    Cmd.map Wizard.Msgs.KMEditorMsg <|
                        Wizard.KMEditor.Update.fetchData route model.kmEditorModel model.appState

                Routes.KnowledgeModelsRoute route ->
                    Cmd.map Wizard.Msgs.KnowledgeModelsMsg <|
                        Wizard.KnowledgeModels.Update.fetchData route model.appState

                Routes.OrganizationRoute ->
                    Cmd.map Wizard.Msgs.OrganizationMsg <|
                        Wizard.Organization.Update.fetchData model.appState

                Routes.PublicRoute route ->
                    Cmd.map Wizard.Msgs.PublicMsg <|
                        Wizard.Public.Update.fetchData route model.appState

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Wizard.Msgs.OnUrlChange location ->
            let
                newModel =
                    setRoute (parseLocation model.appState location) model
                        |> initLocalModel
            in
            ( newModel, fetchData newModel )

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

        Wizard.Msgs.AuthMsg authMsg ->
            Wizard.Auth.Update.update authMsg model

        Wizard.Msgs.SetSidebarCollapsed collapsed ->
            let
                newSession =
                    Session.setSidebarCollapsed model.appState.session collapsed

                newModel =
                    setSession newSession model
            in
            ( newModel, Ports.storeSession <| Just newSession )

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

        Wizard.Msgs.QuestionnairesMsg dsPlannerMsg ->
            let
                ( questionnairesModel, cmd ) =
                    Wizard.Questionnaires.Update.update Wizard.Msgs.QuestionnairesMsg dsPlannerMsg model.appState model.questionnairesModel
            in
            ( { model | questionnairesModel = questionnairesModel }, cmd )

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

        Wizard.Msgs.OrganizationMsg organizationMsg ->
            let
                ( organizationModel, cmd ) =
                    Wizard.Organization.Update.update organizationMsg Wizard.Msgs.OrganizationMsg model.appState model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )

        Wizard.Msgs.PublicMsg publicMsg ->
            let
                ( publicModel, cmd ) =
                    Wizard.Public.Update.update publicMsg Wizard.Msgs.PublicMsg model.appState model.publicModel
            in
            ( { model | publicModel = publicModel }, cmd )

        Wizard.Msgs.UsersMsg usersMsg ->
            let
                ( seed, users, cmd ) =
                    Wizard.Users.Update.update usersMsg Wizard.Msgs.UsersMsg model.appState model.users
            in
            ( setSeed seed { model | users = users }, cmd )
