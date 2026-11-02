module Registry.Pages.Confirmation exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| The link the registry e-mails after signing up: following it activates the
account, at once, and says whether it did.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Gettext exposing (gettext)
import Html exposing (Html, p)
import Registry.Api as Api
import Registry.Data.AppState exposing (AppState)
import Registry.Routes as Routes
import Ui.Alert as Alert
import Ui.Link as Link
import Ui.Page.Loading as Loading


type alias Model =
    { result : ActionResult () }


init : AppState -> { uuid : String, hash : String } -> ( Model, Cmd Msg )
init appState link =
    ( { result = Loading }, Api.putUserActivation appState link Completed )


type Msg
    = Completed (Result ApiError ())


update : AppState -> Msg -> Model -> Model
update appState msg model =
    case msg of
        Completed (Ok _) ->
            { model | result = Success () }

        Completed (Err error) ->
            { model | result = ApiError.toActionResult appState (gettext "Unable to activate your account." appState.locale) error }


view : AppState -> Model -> { title : String, content : List (Html Msg) }
view appState model =
    { title = gettext "Activation" appState.locale
    , content =
        case model.result of
            Success _ ->
                [ Alert.toHtml (Alert.success (gettext "Your account has been activated. You can now log in." appState.locale))
                , p [] [ Link.link { label = gettext "Log in" appState.locale, url = Routes.toUrl (Routes.Login Nothing) } |> Link.toHtml ]
                ]

            Error message ->
                [ Alert.toHtml (Alert.danger message) ]

            _ ->
                [ Loading.toHtml (gettext "Loading..." appState.locale) ]
    }
