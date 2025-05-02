module Wizard.Locales.Create.Models exposing
    ( ContentFiles
    , Model
    , combineContentFiles
    , initialModel
    )

import ActionResult exposing (ActionResult)
import File exposing (File)
import Form exposing (Form)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Dropzone as Dropzone
import Wizard.Locales.Common.LocaleCreateForm as LocaleCreateFrom exposing (LocaleCreateForm)


type alias Model =
    { wizardContent : Maybe File
    , mailContent : Maybe File
    , wizardContentFileDropzone : Dropzone.State
    , mailContentFileDropzone : Dropzone.State
    , form : Form FormError LocaleCreateForm
    , creatingLocale : ActionResult String
    }


type alias ContentFiles =
    { wizard : File
    , mail : File
    }


initialModel : AppState -> Model
initialModel appState =
    { wizardContent = Nothing
    , mailContent = Nothing
    , wizardContentFileDropzone = Dropzone.initialState
    , mailContentFileDropzone = Dropzone.initialState
    , form = LocaleCreateFrom.init appState
    , creatingLocale = ActionResult.Unset
    }


combineContentFiles : Model -> Maybe ContentFiles
combineContentFiles model =
    Maybe.map2 ContentFiles
        model.wizardContent
        model.mailContent
