module WizardResearch exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (href)
import Json.Decode exposing (Value)
import Shared.Utils exposing (dispatch)
import Url exposing (Url)
import WizardResearch.Common.AppState as AppState exposing (AppState)
import WizardResearch.Common.Session as Session exposing (Session)
import WizardResearch.Page as Page
import WizardResearch.Pages.Auth as Auth
import WizardResearch.Pages.Login as Login
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
    case model.pageModel of
        Auth authModel ->
            viewPage Page.Other AuthMsg (Auth.view model.appState authModel)

        Dashboard _ ->
            viewPage Page.Dashboard
                DashboardMsg
                { title = "Dashboard"
                , content =
                    div []
                        [ text "Dashboard"
                        , a [ href (Route.toString Route.Logout) ] [ text "Log out" ]
                        ]
                }

        Login loginModel ->
            viewPage Page.Login LoginMsg (Login.view model.appState loginModel)

        NotFound ->
            Page.view model.appState Page.Other { title = "Page not found", content = div [] [ text "Not Found" ] }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
