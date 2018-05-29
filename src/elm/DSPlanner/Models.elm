module DSPlanner.Models exposing (..)

import DSPlanner.Create.Models
import DSPlanner.Detail.Models
import DSPlanner.Index.Models
import DSPlanner.Routing exposing (Route(..))


type alias Model =
    { createModel : DSPlanner.Create.Models.Model
    , detailModel : DSPlanner.Detail.Models.Model
    , indexModel : DSPlanner.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = DSPlanner.Create.Models.initialModel Nothing
    , detailModel = DSPlanner.Detail.Models.initialModel ""
    , indexModel = DSPlanner.Index.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        Create selectedPackage ->
            { model | createModel = DSPlanner.Create.Models.initialModel selectedPackage }

        Detail uuid ->
            { model | detailModel = DSPlanner.Detail.Models.initialModel uuid }

        Index ->
            { model | indexModel = DSPlanner.Index.Models.initialModel }
