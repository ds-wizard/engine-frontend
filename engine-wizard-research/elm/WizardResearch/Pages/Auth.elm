module WizardResearch.Pages.Auth exposing (Model, Msg, init, update, view)

-- MODEL

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, div, p, text)
import Shared.Api.Auth as AuthApi
import Shared.Api.Users as UsersApi
import Shared.Data.Token as Token exposing (Token)
import Shared.Data.UserInfo exposing (UserInfo)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode)
import WizardResearch.Common.AppState exposing (AppState)
import WizardResearch.Common.Session exposing (Session)


type alias Model =
    { rawToken : String
    , token : ActionResult Token
    , userInfo : ActionResult UserInfo
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
    | GetUserInfoComplete (Result ApiError UserInfo)


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

        GetUserInfoComplete result ->
            handleGetUserInfoComplete cfg result model


updateWith : UpdateConfig msg -> ( Model, Cmd Msg ) -> ( Model, Cmd msg )
updateWith cfg ( model, cmd ) =
    ( model, Cmd.map cfg.wrapMsg cmd )


handleGetTokenComplete : AppState -> Result ApiError Token -> Model -> ( Model, Cmd Msg )
handleGetTokenComplete appState result model =
    case result of
        Ok token ->
            let
                rawToken =
                    Token.value token

                apiConfig =
                    { apiUrl = appState.apiConfig.apiUrl, token = rawToken }
            in
            ( { model | userInfo = Loading, token = Success token, rawToken = rawToken }
            , UsersApi.getUserInfo { appState | apiConfig = apiConfig } GetUserInfoComplete
            )

        Err error ->
            ( { model | token = ApiError.toActionResult "Unable to get token" error }
            , Cmd.none
            )


handleGetUserInfoComplete : UpdateConfig msg -> Result ApiError UserInfo -> Model -> ( Model, Cmd msg )
handleGetUserInfoComplete cfg result model =
    case result of
        Ok userInfo ->
            ( model
            , cfg.onAuthenticate <| Session model.rawToken userInfo
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
