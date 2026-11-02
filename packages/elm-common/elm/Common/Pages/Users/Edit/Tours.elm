module Common.Pages.Users.Edit.Tours exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Tours as ToursApi
import Common.Components.Tour as Tour
import Common.Data.AppState exposing (AppState)
import Common.Data.Session as Session
import Common.Data.TourId as TourId
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Users.Components.Section as Section
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Layout as Layout
import Ui.Text as Text



-- MODEL


type alias Model =
    { resettingTours : ActionResult String
    }


initialModel : Model
initialModel =
    { resettingTours = Unset
    }


{-| Starts the page's own tour.
-}
fetchData : AppState -> Cmd msg
fetchData appState =
    Tour.tour appState
        { tourId = TourId.usersEditTours
        , completedTours = appState.config.tours
        , enabled = appState.config.features.toursEnabled && Session.exists appState.session
        , steps =
            [ { element = Tour.centered
              , title = gettext "Tours" appState.locale
              , description = gettext "Tours are interactive guides that highlight key features on selected pages to help you get familiar with the interface." appState.locale
              }
            , { element = Tour.byTestId resetTestId
              , title = gettext "Reset Tours" appState.locale
              , description = gettext "This will reset all previously completed tours, allowing them to restart as if they were never viewed." appState.locale
              }
            ]
        }


resetTestId : String
resetTestId =
    "tours_reset"



-- UPDATE


type Msg
    = ResetTours
    | ResetToursCompleted (Result ApiError ())


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ResetTours ->
            ( { model | resettingTours = Loading }
            , ToursApi.resetTours appState (cfg.wrapMsg << ResetToursCompleted)
            )

        ResetToursCompleted result ->
            let
                resettingResult =
                    case result of
                        Ok _ ->
                            Success (gettext "Tours have been reset." appState.locale)

                        Err error ->
                            ApiError.toActionResult appState (gettext "Tours could not be reset." appState.locale) error
            in
            ( { model | resettingTours = resettingResult }
            , RequestHelpers.getResultCmd cfg.logoutMsg result
            )



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    let
        resultAlert =
            case model.resettingTours of
                Success message ->
                    Alert.toHtml (Alert.success message)

                Error message ->
                    Alert.toHtml (Alert.danger message)

                _ ->
                    Html.text ""
    in
    Section.view appState
        { title = gettext "Tours" appState.locale
        , guideLink = Nothing
        , content =
            [ resultAlert
            , Text.body (gettext "This resets all page-specific onboarding tours, allowing the guided highlights and instructions to replay on next visit." appState.locale)
            , Layout.inline
                [ Button.primary (gettext "Reset" appState.locale) ResetTours
                    |> Button.withLoading (ActionResult.isLoading model.resettingTours)
                    |> Button.withTestId resetTestId
                    |> Button.toHtml
                ]
            ]
        }
