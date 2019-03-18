module Questionnaires.Models exposing (Model, initLocalModel, initialModel)

import Questionnaires.Create.Models
import Questionnaires.Detail.Models
import Questionnaires.Edit.Models
import Questionnaires.Index.Models
import Questionnaires.Routing exposing (Route(..))


type alias Model =
    { createModel : Questionnaires.Create.Models.Model
    , detailModel : Questionnaires.Detail.Models.Model
    , editModel : Questionnaires.Edit.Models.Model
    , indexModel : Questionnaires.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Questionnaires.Create.Models.initialModel Nothing
    , detailModel = Questionnaires.Detail.Models.initialModel ""
    , editModel = Questionnaires.Edit.Models.initialModel ""
    , indexModel = Questionnaires.Index.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        Create selectedPackage ->
            { model | createModel = Questionnaires.Create.Models.initialModel selectedPackage }

        Detail uuid ->
            { model | detailModel = Questionnaires.Detail.Models.initialModel uuid }

        Edit uuid ->
            { model | editModel = Questionnaires.Edit.Models.initialModel uuid }

        Index ->
            { model | indexModel = Questionnaires.Index.Models.initialModel }
