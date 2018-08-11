module Subscriptions exposing (..)

import Common.Menu.Subscriptions
import DSPlanner.Subscriptions
import KMEditor.Subscriptions
import KMPackages.Subscriptions
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Routing exposing (Route(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        currentViewSubscriptions =
            case model.route of
                DSPlanner route ->
                    DSPlanner.Subscriptions.subscriptions DSPlannerMsg route model.dsPlannerModel

                KMEditor route ->
                    KMEditor.Subscriptions.subscriptions KMEditorMsg route model.kmEditorModel

                KMPackages route ->
                    KMPackages.Subscriptions.subscriptions KMPackagesMsg route model.kmPackagesModel

                _ ->
                    Sub.none

        menuSubscriptions =
            Common.Menu.Subscriptions.subscriptions model.menuModel
    in
    Sub.batch [ currentViewSubscriptions, menuSubscriptions ]
