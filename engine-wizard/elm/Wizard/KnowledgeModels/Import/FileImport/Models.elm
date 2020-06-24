module Wizard.KnowledgeModels.Import.FileImport.Models exposing
    ( Model
    , dropzoneId
    , fileInputId
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.FilePortData exposing (FilePortData)


type alias Model =
    { dnd : Int
    , file : Maybe FilePortData
    , importing : ActionResult String
    }


initialModel : Model
initialModel =
    { dnd = 0
    , file = Nothing
    , importing = Unset
    }


dropzoneId : String
dropzoneId =
    "km-import-dropzone"


fileInputId : String
fileInputId =
    "km-import-input"
