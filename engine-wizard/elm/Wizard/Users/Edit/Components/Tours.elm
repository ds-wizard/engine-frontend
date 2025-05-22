module Wizard.Users.Edit.Components.Tours exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Shared.Api.Tours as ToursApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Driver as Driver exposing (TourConfig)
import Wizard.Common.Html.Attribute exposing (dataTour, selectDataTour)
import Wizard.Common.TourId as TourId
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page


type alias Model =
    { resettingTours : ActionResult String
    }


initialModel : Model
initialModel =
    { resettingTours = ActionResult.Unset
    }


fetchData : AppState -> Cmd Msg
fetchData appState =
    Driver.init (tour appState)


tour : AppState -> TourConfig
tour appState =
    Driver.tourConfig TourId.usersEditTours appState.locale
        |> Driver.addCompletedTourIds appState.config.tours
        |> Driver.addStep
            { element = Nothing
            , popover =
                { title = gettext "Tours" appState.locale
                , description = gettext "Tours are interactive guides that highlight key features on selected pages to help you get familiar with the interface." appState.locale
                }
            }
        |> Driver.addStep
            { element = selectDataTour "tours_reset"
            , popover =
                { title = gettext "Reset Tours" appState.locale
                , description = gettext "This will reset all previously completed tours, allowing them to restart as if they were never viewed." appState.locale
                }
            }


type Msg
    = ResetTours
    | ResetToursCompleted (Result ApiError ())


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ResetTours ->
            ( { model | resettingTours = ActionResult.Loading }
            , ToursApi.resetTours appState (cfg.wrapMsg << ResetToursCompleted)
            )

        ResetToursCompleted result ->
            let
                resettingResult =
                    case result of
                        Ok _ ->
                            ActionResult.Success (gettext "Tours have been reset." appState.locale)

                        Err error ->
                            ApiError.toActionResult appState (gettext "Tours could not be reset." appState.locale) error

                cmd =
                    getResultCmd cfg.logoutMsg result
            in
            ( { model | resettingTours = resettingResult }, cmd )


view : AppState -> Model -> Html Msg
view appState model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-8" ]
                [ Page.header (gettext "Tours" appState.locale) []
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-8" ]
                [ FormResult.view appState model.resettingTours
                , p [] [ text (gettext "This resets all page-specific onboarding tours, allowing the guided highlights and instructions to replay on next visit." appState.locale) ]
                , div []
                    [ ActionButton.buttonWithAttrs appState
                        { label = gettext "Reset" appState.locale
                        , result = model.resettingTours
                        , msg = ResetTours
                        , dangerous = False
                        , attrs = [ dataTour "tours_reset" ]
                        }
                    ]
                ]
            ]
        ]
