module Wizard.Pages.Users.Edit.Components.Tours exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.ActionButton as ActionButton
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Utils.Driver as Driver exposing (TourConfig)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataTour, selectDataTour)
import Wizard.Api.Tours as ToursApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Utils.Driver as Driver
import Wizard.Utils.TourId as TourId


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
    Driver.fromAppState TourId.usersEditTours appState
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
                    RequestHelpers.getResultCmd cfg.logoutMsg result
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
                [ FormResult.view model.resettingTours
                , p [] [ text (gettext "This resets all page-specific onboarding tours, allowing the guided highlights and instructions to replay on next visit." appState.locale) ]
                , div []
                    [ ActionButton.buttonWithAttrs
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
