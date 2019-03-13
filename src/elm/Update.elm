module Update exposing (fetchData, update)

import Auth.Models exposing (setSidebarCollapsed)
import Auth.Update
import Browser
import Browser.Navigation exposing (load, pushUrl)
import Common.Menu.Update
import KMEditor.Update
import KnowledgeModels.Update
import Models exposing (Model, initLocalModel, setRoute, setSeed, setSession)
import Msgs exposing (Msg)
import Organization.Update
import Ports
import Public.Update
import Questionnaires.Update
import Routing exposing (Route(..), isAllowed, parseLocation)
import Url exposing (Url)
import Users.Update


fetchData : Model -> Cmd Msg
fetchData model =
    case model.state.route of
        Questionnaires route ->
            Questionnaires.Update.fetchData route Msgs.QuestionnairesMsg model.state.session model.dsPlannerModel

        KMEditor route ->
            KMEditor.Update.fetchData route Msgs.KMEditorMsg model.kmEditorModel model.state.session

        KnowledgeModels route ->
            KnowledgeModels.Update.fetchData route Msgs.KnowledgeModelsMsg model.state.session

        Organization ->
            Organization.Update.getCurrentOrganizationCmd model.state.session

        Public route ->
            Public.Update.fetchData route Msgs.PublicMsg

        Users route ->
            Users.Update.fetchData route Msgs.UsersMsg model.state.session

        _ ->
            Cmd.none


isGuarded : Model -> Maybe String
isGuarded model =
    case model.state.route of
        KMEditor route ->
            KMEditor.Update.isGuarded route model.kmEditorModel

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnUrlChange location ->
            let
                newModel =
                    setRoute (parseLocation location) model
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
                            ( model, pushUrl model.state.key (Url.toString url) )

                Browser.External url ->
                    if url == "" then
                        ( model, Cmd.none )

                    else
                        ( model, load url )

        Msgs.AuthMsg authMsg ->
            Auth.Update.update authMsg model

        Msgs.SetSidebarCollapsed collapsed ->
            let
                newSession =
                    setSidebarCollapsed model.state.session collapsed

                newModel =
                    setSession newSession model
            in
            ( newModel, Ports.storeSession <| Just newSession )

        Msgs.MenuMsg menuMsg ->
            let
                ( menuModel, cmd ) =
                    Common.Menu.Update.update Msgs.MenuMsg menuMsg model.menuModel
            in
            ( { model | menuModel = menuModel }, cmd )

        Msgs.QuestionnairesMsg dsPlannerMsg ->
            let
                ( dsPlannerModel, cmd ) =
                    Questionnaires.Update.update dsPlannerMsg Msgs.QuestionnairesMsg model.state model.dsPlannerModel
            in
            ( { model | dsPlannerModel = dsPlannerModel }, cmd )

        Msgs.KMEditorMsg kmEditorMsg ->
            let
                ( seed, kmEditorModel, cmd ) =
                    KMEditor.Update.update kmEditorMsg Msgs.KMEditorMsg model.state model.kmEditorModel
            in
            ( setSeed seed { model | kmEditorModel = kmEditorModel }, cmd )

        Msgs.KnowledgeModelsMsg kmPackagesMsg ->
            let
                ( kmPackagesModel, cmd ) =
                    KnowledgeModels.Update.update kmPackagesMsg Msgs.KnowledgeModelsMsg model.state model.kmPackagesModel
            in
            ( { model | kmPackagesModel = kmPackagesModel }, cmd )

        Msgs.OrganizationMsg organizationMsg ->
            let
                ( organizationModel, cmd ) =
                    Organization.Update.update organizationMsg model.state.session model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )

        Msgs.PublicMsg publicMsg ->
            let
                ( seed, publicModel, cmd ) =
                    Public.Update.update publicMsg Msgs.PublicMsg model.state model.publicModel
            in
            ( setSeed seed { model | publicModel = publicModel }, cmd )

        Msgs.UsersMsg usersMsg ->
            let
                ( seed, users, cmd ) =
                    Users.Update.update usersMsg Msgs.UsersMsg model.state model.users
            in
            ( setSeed seed { model | users = users }, cmd )
