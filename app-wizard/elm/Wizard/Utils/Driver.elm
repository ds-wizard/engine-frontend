module Wizard.Utils.Driver exposing (fromAppState)

import Common.Data.Session as Session
import Common.Utils.Driver as Driver exposing (TourConfig, TourId)
import Wizard.Data.AppState exposing (AppState)


fromAppState : TourId -> AppState -> TourConfig
fromAppState tourId appState =
    Driver.tourConfig tourId appState.locale
        |> Driver.addToursEnabled appState.config.features.toursEnabled
        |> Driver.addCompletedTourIds appState.config.tours
        |> Driver.addLoggedIn (Session.exists appState.session)
