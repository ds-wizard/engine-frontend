module Update exposing (fetchData, update)

import Auth.Models exposing (setSidebarCollapsed)
import Auth.Update
import Browser
import Browser.Navigation exposing (load, pushUrl)
import Common.AppState as AppState
import Common.Menu.Update
import Common.Time as Time
import Dashboard.Update
import KMEditor.Update
import KnowledgeModels.Update
import Models exposing (Model, initLocalModel, setRoute, setSeed, setSession)
import Msgs exposing (Msg(..))
import Organization.Update
import Ports
import Public.Update
import Questionnaires.Update
import Routes
import Routing exposing (parseLocation)
import Url exposing (Url)
import Users.Update


fetchData : Model -> Cmd Msg
fetchData model =
    let
        fetchCmd =
            case model.appState.route of
                Routes.DashboardRoute ->
                    Cmd.map DashboardMsg <|
                        Dashboard.Update.fetchData model.appState

                Routes.QuestionnairesRoute route ->
                    Cmd.map Msgs.QuestionnairesMsg <|
                        Questionnaires.Update.fetchData route model.appState model.questionnairesModel

                Routes.KMEditorRoute route ->
                    Cmd.map Msgs.KMEditorMsg <|
                        KMEditor.Update.fetchData route model.kmEditorModel model.appState

                Routes.KnowledgeModelsRoute route ->
                    Cmd.map Msgs.KnowledgeModelsMsg <|
                        KnowledgeModels.Update.fetchData route model.appState

                Routes.OrganizationRoute ->
                    Cmd.map Msgs.OrganizationMsg <|
                        Organization.Update.fetchData model.appState

                Routes.PublicRoute route ->
                    Cmd.map Msgs.PublicMsg <|
                        Public.Update.fetchData route model.appState

                Routes.UsersRoute route ->
                    Cmd.map Msgs.UsersMsg <|
                        Users.Update.fetchData route model.appState

                _ ->
                    Cmd.none
    in
    Cmd.batch [ fetchCmd, Time.getTime ]


isGuarded : Model -> Maybe String
isGuarded model =
    case model.appState.route of
        Routes.KMEditorRoute route ->
            KMEditor.Update.isGuarded route model.appState model.kmEditorModel

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnUrlChange location ->
            let
                newModel =
                    setRoute (parseLocation model.appState location) model
                        |> initLocalModel
            in
            ( newModel, fetchData newModel )

        Msgs.OnUrlRequest urlRequest ->
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

        Msgs.OnTime time ->
            ( { model | appState = AppState.setCurrentTime model.appState time }, Cmd.none )

        Msgs.AuthMsg authMsg ->
            Auth.Update.update authMsg model

        Msgs.SetSidebarCollapsed collapsed ->
            let
                newSession =
                    setSidebarCollapsed model.appState.session collapsed

                newModel =
                    setSession newSession model
            in
            ( newModel, Ports.storeSession <| Just newSession )

        Msgs.MenuMsg menuMsg ->
            let
                ( menuModel, cmd ) =
                    Common.Menu.Update.update Msgs.MenuMsg menuMsg model.appState model.menuModel
            in
            ( { model | menuModel = menuModel }, cmd )

        Msgs.DashboardMsg dashboardMsg ->
            let
                ( dashboardModel, cmd ) =
                    Dashboard.Update.update dashboardMsg model.appState model.dashboardModel
            in
            ( { model | dashboardModel = dashboardModel }, cmd )

        Msgs.QuestionnairesMsg dsPlannerMsg ->
            let
                ( questionnairesModel, cmd ) =
                    Questionnaires.Update.update Msgs.QuestionnairesMsg dsPlannerMsg model.appState model.questionnairesModel
            in
            ( { model | questionnairesModel = questionnairesModel }, cmd )

        Msgs.KMEditorMsg kmEditorMsg ->
            let
                ( seed, kmEditorModel, cmd ) =
                    KMEditor.Update.update kmEditorMsg Msgs.KMEditorMsg model.appState model.kmEditorModel
            in
            ( setSeed seed { model | kmEditorModel = kmEditorModel }, cmd )

        Msgs.KnowledgeModelsMsg kmPackagesMsg ->
            let
                ( kmPackagesModel, cmd ) =
                    KnowledgeModels.Update.update kmPackagesMsg Msgs.KnowledgeModelsMsg model.appState model.kmPackagesModel
            in
            ( { model | kmPackagesModel = kmPackagesModel }, cmd )

        Msgs.OrganizationMsg organizationMsg ->
            let
                ( organizationModel, cmd ) =
                    Organization.Update.update organizationMsg Msgs.OrganizationMsg model.appState model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )

        Msgs.PublicMsg publicMsg ->
            let
                ( publicModel, cmd ) =
                    Public.Update.update publicMsg Msgs.PublicMsg model.appState model.publicModel
            in
            ( { model | publicModel = publicModel }, cmd )

        Msgs.UsersMsg usersMsg ->
            let
                ( seed, users, cmd ) =
                    Users.Update.update usersMsg Msgs.UsersMsg model.appState model.users
            in
            ( setSeed seed { model | users = users }, cmd )
