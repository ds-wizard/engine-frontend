module Wizard.Pages.DocumentTemplates.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.DocumentTemplateLocale exposing (DocumentTemplateLocale)
import Wizard.Pages.DocumentTemplates.Detail.DocumentTemplateDetailRoute exposing (DocumentTemplateDetailRoute)
import Wizard.Pages.DocumentTemplates.Detail.ImportLocaleModal as ImportLocaleModal


type alias Model =
    { template : ActionResult DocumentTemplateDetail
    , detailRoute : DocumentTemplateDetailRoute
    , dropdownState : Dropdown.State
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    , showAllKms : Bool
    , importLocaleModal : ImportLocaleModal.Model
    , localeToDelete : Maybe DocumentTemplateLocale
    , deletingLocale : ActionResult String
    }


initialModel : DocumentTemplateDetailRoute -> Model
initialModel detailRoute =
    { template = Loading
    , detailRoute = detailRoute
    , dropdownState = Dropdown.initialState
    , deletingVersion = Unset
    , showDeleteDialog = False
    , showAllKms = False
    , importLocaleModal = ImportLocaleModal.init
    , localeToDelete = Nothing
    , deletingLocale = Unset
    }
