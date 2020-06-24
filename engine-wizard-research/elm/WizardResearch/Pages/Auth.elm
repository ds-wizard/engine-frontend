module WizardResearch.Pages.Auth exposing (Model, Msg, init, update, view)

-- MODEL

import ActionResult exposing (ActionResult(..))
import Html.Styled exposing (Html, div, p, text)
import Shared.Api.Auth as AuthApi
import Shared.Api.Users as UsersApi
import Shared.Auth.Session as Session exposing (Session)
import Shared.Data.Token exposing (Token)
import Shared.Data.User as User exposing (User)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html.Styled exposing (emptyNode)
import Shared.Setters exposing (setToken, setUser)
import WizardResearch.Common.AppState exposing (AppState)


type alias Model =
    { rawToken : String
    , token : ActionResult Token
    , userInfo : ActionResult User
    }


init : AppState -> String -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
init appState id mbError mbCode =
    ( { rawToken = ""
      , token = Loading
      , userInfo = Unset
      }
    , AuthApi.getToken id mbError mbCode appState GetTokenComplete
    )



-- UPDATE


type Msg
    = GetTokenComplete (Result ApiError Token)
    | GetUserComplete (Result ApiError User)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , onAuthenticate : Session -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetTokenComplete result ->
            updateWith cfg <|
                handleGetTokenComplete appState result model

        GetUserComplete result ->
            handleGetUserInfoComplete cfg result model


updateWith : UpdateConfig msg -> ( Model, Cmd Msg ) -> ( Model, Cmd msg )
updateWith cfg ( model, cmd ) =
    ( model, Cmd.map cfg.wrapMsg cmd )


handleGetTokenComplete : AppState -> Result ApiError Token -> Model -> ( Model, Cmd Msg )
handleGetTokenComplete appState result model =
    case result of
        Ok token ->
            let
                tempAppState =
                    { appState | session = setToken token appState.session }
            in
            ( { model | userInfo = Loading, token = Success token, rawToken = token.token }
            , UsersApi.getCurrentUser tempAppState GetUserComplete
            )

        Err error ->
            ( { model | token = ApiError.toActionResult "Unable to get token" error }
            , Cmd.none
            )


handleGetUserInfoComplete : UpdateConfig msg -> Result ApiError User -> Model -> ( Model, Cmd msg )
handleGetUserInfoComplete cfg result model =
    case result of
        Ok user ->
            let
                session =
                    Session.init
                        |> setToken { token = model.rawToken }
                        |> setUser (Just (User.toUserInfo user))
            in
            ( model
            , cfg.onAuthenticate session
            )

        Err error ->
            ( { model | userInfo = ApiError.toActionResult "Unable to get user info" error }
            , Cmd.none
            )



-- VIEW


view : AppState -> Model -> { title : String, content : Html Msg }
view appState model =
    let
        actionResults =
            ActionResult.combine model.token model.userInfo

        result =
            case actionResults of
                Error e ->
                    p [] [ text e ]

                Loading ->
                    p [] [ text "Loading..." ]

                _ ->
                    emptyNode
    in
    { title = "Authentication"
    , content = div [] [ result ]
    }
