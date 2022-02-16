module Wizard.KnowledgeModels.Import.OwlImport.Models exposing
    ( Model
    , dropzoneId
    , fileInputId
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import File exposing (File)
import Form exposing (Form)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Common.OwlImportForm as OwlImportForm exposing (OwlImportForm)


type alias Model =
    { dnd : Int
    , file : Maybe File
    , form : Form FormError OwlImportForm
    , importing : ActionResult String
    }


initialModel : AppState -> Model
initialModel appState =
    { dnd = 0
    , file = Nothing
    , importing = Unset
    , form = OwlImportForm.init appState
    }


dropzoneId : String
dropzoneId =
    "owl-import-dropzone"


fileInputId : String
fileInputId =
    "owl-import-input"
