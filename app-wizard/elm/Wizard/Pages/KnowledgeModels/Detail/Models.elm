module Wizard.Pages.KnowledgeModels.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Wizard.Api.Models.KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Pages.KnowledgeModels.Common.DeleteModal as DeleteModal
import Wizard.Pages.KnowledgeModels.Detail.ImportLocaleModal as ImportLocaleModal
import Wizard.Pages.KnowledgeModels.Detail.KnowledgeModelDetailRoute exposing (KnowledgeModelDetailRoute)


type alias Model =
    { knowledgeModelPackage : ActionResult KnowledgeModelPackageDetail
    , detailRoute : KnowledgeModelDetailRoute
    , dropdownState : Dropdown.State
    , deleteModalModel : DeleteModal.Model
    , importLocaleModal : ImportLocaleModal.Model
    , localeToDelete : Maybe KnowledgeModelLocale
    , deletingLocale : ActionResult String
    , showAllVersions : Bool
    }


initialModel : KnowledgeModelDetailRoute -> Model
initialModel detailRoute =
    { knowledgeModelPackage = Loading
    , detailRoute = detailRoute
    , dropdownState = Dropdown.initialState
    , deleteModalModel = DeleteModal.initialModel False
    , importLocaleModal = ImportLocaleModal.init
    , localeToDelete = Nothing
    , deletingLocale = Unset
    , showAllVersions = False
    }
