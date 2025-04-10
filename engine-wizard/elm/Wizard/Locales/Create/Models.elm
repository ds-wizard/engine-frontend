module Wizard.Locales.Create.Models exposing
    ( Model
    , dropzoneId
    , fileInputId
    , initialModel
    )

import ActionResult exposing (ActionResult)
import File exposing (File)
import Form exposing (Form)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Locales.Common.LocaleCreateForm as LocaleCreateFrom exposing (LocaleCreateFrom)


type alias Model =
    { dnd : Int
    , file : Maybe File
    , form : Form FormError LocaleCreateFrom
    , creatingLocale : ActionResult String
    }


initialModel : AppState -> Model
initialModel appState =
    { dnd = 0
    , file = Nothing
    , form = LocaleCreateFrom.init appState
    , creatingLocale = ActionResult.Unset
    }


dropzoneId : String
dropzoneId =
    "locale-import-dropzone"


fileInputId : String
fileInputId =
    "locale-import-input"
