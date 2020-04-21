module WizardResearch exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Css exposing (backgroundColor, height, hex, pct, px, width)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Json.Decode exposing (Value)
import Shared.Elemental.Atoms.Button as Button
import Shared.Elemental.Atoms.Heading as Heading
import Shared.Elemental.Foundations.Grid as Grid
import Shared.Elemental.Foundations.Illustration as Illustration
import Shared.Html.Styled exposing (fa)
import Shared.Utils exposing (dispatch)
import Url exposing (Url)
import WizardResearch.Common.AppState as AppState exposing (AppState)
import WizardResearch.Common.Session as Session exposing (Session)
import WizardResearch.Page as Page
import WizardResearch.Pages.Auth as Auth
import WizardResearch.Pages.Login as Login
import WizardResearch.Pages.Project as Project
import WizardResearch.Pages.ProjectCreate as ProjectCreate
import WizardResearch.Ports as Ports
import WizardResearch.Route as Route exposing (Route)


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { appState : AppState
    , route : Route
    , navKey : Nav.Key
    , pageModel : PageModel
    }


type PageModel
    = Auth Auth.Model
    | Dashboard ()
    | Login Login.Model
    | ProjectCreate ProjectCreate.Model
    | Project Project.Model
    | NotFound


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    initViewModel
        { appState = AppState.init flags
        , route = Route.fromUrl url
        , navKey = navKey
        , pageModel = NotFound
        }


initViewModel : Model -> ( Model, Cmd Msg )
initViewModel model =
    if Route.isPublic model.route || AppState.authenticated model.appState then
        case model.route of
            Route.AuthCallback id mbError mbCode ->
                Auth.init model.appState id mbError mbCode
                    |> updateWith Auth AuthMsg model

            Route.Dashboard ->
                ( (), Cmd.none )
                    |> updateWith Dashboard DashboardMsg model

            Route.Login ->
                if AppState.authenticated model.appState then
                    ( model, Route.replaceUrl model.navKey Route.Dashboard )

                else
                    Login.init
                        |> updateWith Login LoginMsg model

            Route.Logout ->
                ( { model | appState = AppState.setSession Nothing model.appState }
                , Cmd.batch
                    [ Route.replaceUrl model.navKey Route.Login
                    , Ports.clearSession ()
                    ]
                )

            Route.NotFound ->
                ( { model | pageModel = NotFound }, Cmd.none )

            Route.ProjectCreate ->
                ProjectCreate.init model.appState
                    |> updateWith ProjectCreate ProjectCreateMsg model

            Route.Project uuid ->
                Project.init model.appState uuid
                    |> updateWith Project ProjectMsg model

    else
        ( model, Route.replaceUrl model.navKey Route.Login )



-- UPDATE


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnAuthenticate Session
    | AuthMsg Auth.Msg
    | DashboardMsg ()
    | LoginMsg Login.Msg
    | ProjectCreateMsg ProjectCreate.Msg
    | ProjectMsg Project.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.pageModel ) of
        ( OnUrlChange url, _ ) ->
            initViewModel { model | route = Route.fromUrl url }

        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( OnAuthenticate session, _ ) ->
            ( { model | appState = AppState.setSession (Just session) model.appState }
            , Cmd.batch
                [ Route.replaceUrl model.navKey Route.Dashboard
                , Ports.storeSession (Session.encode session)
                ]
            )

        ( AuthMsg authMsg, Auth authModel ) ->
            let
                updateConfig =
                    { wrapMsg = AuthMsg
                    , onAuthenticate = dispatch << OnAuthenticate
                    }
            in
            Auth.update updateConfig model.appState authMsg authModel
                |> updateWith Auth identity model

        ( DashboardMsg _, Dashboard _ ) ->
            ( model, Cmd.none )

        ( LoginMsg loginMsg, Login loginModel ) ->
            let
                updateConfig =
                    { wrapMsg = LoginMsg
                    , onAuthenticate = dispatch << OnAuthenticate
                    }
            in
            Login.update updateConfig model.appState loginMsg loginModel
                |> updateWith Login identity model

        ( ProjectCreateMsg projectCreateMsg, ProjectCreate projectCreateModel ) ->
            let
                updateConfig =
                    { wrapMsg = ProjectCreateMsg
                    , cmdNavigate = Nav.pushUrl model.navKey << Route.toString
                    }
            in
            ProjectCreate.update updateConfig model.appState projectCreateMsg projectCreateModel
                |> updateWith ProjectCreate identity model

        ( ProjectMsg projectMsg, Project projectModel ) ->
            Project.update model.appState projectMsg projectModel
                |> updateWith Project ProjectMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith :
    (subModel -> PageModel)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | pageModel = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view model.appState page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    if model.appState.configurationError then
        Page.view model.appState
            Page.Public
            { title = "Configuration Error"
            , content = div [] [ text "Configuration Error" ]
            }

    else
        case model.pageModel of
            Auth authModel ->
                viewPage Page.Public AuthMsg (Auth.view model.appState authModel)

            Dashboard _ ->
                viewPage Page.App
                    DashboardMsg
                    { title = "Dashboard"
                    , content =
                        Grid.comfortable.container
                            [ Grid.containerLimitedSmall ]
                            [ Grid.comfortable.row []
                                [ Grid.comfortable.col 12
                                    []
                                    [ Heading.h1 model.appState.theme "Dashboard"
                                    ]
                                ]
                            , Grid.comfortable.row []
                                [ Grid.comfortable.col 6
                                    []
                                    [ a [ href (Route.toString Route.Logout) ] [ text "Log out" ]
                                    ]
                                , Grid.comfortable.col 6
                                    [ Grid.colTextRight ]
                                    [ Button.primaryLink model.appState.theme
                                        [ href (Route.toString Route.ProjectCreate) ]
                                        [ fa "fas fa-plus"
                                        , span [] [ text "Create project" ]
                                        ]
                                    ]
                                ]
                            ]
                    }

            Login loginModel ->
                viewPage Page.Public LoginMsg (Login.view model.appState loginModel)

            NotFound ->
                Page.view model.appState Page.Auto { title = "Page not found", content = div [] [ text "Not Found" ] }

            ProjectCreate projectCreateModel ->
                viewPage Page.App ProjectCreateMsg (ProjectCreate.view model.appState projectCreateModel)

            Project projectModel ->
                viewPage Page.App ProjectMsg (Project.view model.appState projectModel)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
