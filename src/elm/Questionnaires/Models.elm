module Questionnaires.Models exposing (..)

import Questionnaires.Create.Models
import Questionnaires.Detail.Models
import Questionnaires.Index.Models


type alias Model =
    { createModel : Questionnaires.Create.Models.Model
    , detailModel : Questionnaires.Detail.Models.Model
    , indexModel : Questionnaires.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Questionnaires.Create.Models.initialModel
    , detailModel = Questionnaires.Detail.Models.initialModel
    , indexModel = Questionnaires.Index.Models.initialModel
    }
