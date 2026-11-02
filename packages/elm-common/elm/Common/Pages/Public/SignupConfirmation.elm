module Common.Pages.Public.SignupConfirmation exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| Activate an account from the link in the sign-up email. The request is sent
as soon as the page opens.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Users as UsersApi
import Common.Components.Page as Page
import Common.Data.AppState exposing (AppState)
import Common.Pages.Public.Layout as PublicLayout
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html, p)
import String.Format as String
import Ui.Alert as Alert
import Ui.Link as Link


type alias Model =
    { confirmation : ActionResult ()
    }


initialModel : Model
initialModel =
    { confirmation = Loading
    }


type Msg
    = SendConfirmationCompleted (Result ApiError ())


init : AppState -> String -> String -> ( Model, Cmd Msg )
init appState userId hash =
    ( initialModel
    , UsersApi.putUserActivation appState userId hash SendConfirmationCompleted
    )


update : AppState -> Msg -> Model -> Model
update appState msg model =
    case msg of
        SendConfirmationCompleted result ->
            case result of
                Ok _ ->
                    { model | confirmation = Success () }

                Err error ->
                    { model | confirmation = ApiError.toActionResult appState (gettext "Sign up process failed." appState.locale) error }


view : AppState -> Model -> Html Msg
view appState model =
    case model.confirmation of
        Success _ ->
            PublicLayout.panel appState
                { title = gettext "Sign Up" appState.locale
                , intro = Nothing
                , aside = Nothing
                , content =
                    [ Alert.success ""
                        |> Alert.toHtmlWithContent
                            [ p []
                                (String.formatHtml
                                    (gettext "Your email was successfully confirmed. You can now %s." appState.locale)
                                    [ Link.link { label = gettext "log in" appState.locale, url = Routes.toUrl appState (Routes.publicLogin Nothing) }
                                        |> Link.toHtml
                                    ]
                                )
                            ]
                    ]
                }

        _ ->
            PublicLayout.app appState [ Page.actionResult appState (always (Html.text "")) model.confirmation ]
