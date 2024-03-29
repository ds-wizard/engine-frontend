module Wizard.Locales.Import.FileImport.Models exposing
    ( Model
    , dropzoneId
    , fileInputId
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import File exposing (File)


type alias Model =
    { dnd : Int
    , file : Maybe File
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
    "locale-import-dropzone"


fileInputId : String
fileInputId =
    "locale-import-input"
