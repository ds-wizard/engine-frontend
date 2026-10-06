module Wizard.Pages.DocumentTemplates.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.DocumentTemplateLocale exposing (DocumentTemplateLocale)
import Wizard.Pages.DocumentTemplates.Detail.DocumentTemplateDetailRoute exposing (DocumentTemplateDetailRoute)
import Wizard.Pages.DocumentTemplates.Detail.ImportLocaleModal as ImportLocaleModal


type Msg
    = GetTemplateCompleted (Result ApiError DocumentTemplateDetail)
    | OpenDetailRoute DocumentTemplateDetailRoute
    | DropdownMsg Dropdown.State
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | UpdatePhase DocumentTemplatePhase
    | UpdatePhaseCompleted (Result ApiError DocumentTemplateDetail)
    | ExportTemplate DocumentTemplateDetail
    | ExportTemplatePot DocumentTemplateDetail
    | OpenImportLocaleModal
    | ImportLocaleModalMsg ImportLocaleModal.Msg
    | ShowDeleteLocale DocumentTemplateLocale
    | HideDeleteLocale
    | DeleteLocale
    | DeleteLocaleCompleted (Result ApiError ())
    | DownloadLocale DocumentTemplateLocale
    | DownloadLocaleCompleted DocumentTemplateLocale (Result ApiError String)
    | FileDownloaderMsg FileDownloader.Msg
    | ShowAllKms
