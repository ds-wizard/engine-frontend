module Wizard.Pages.DocumentTemplates.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Components.FileDownloader as FileDownloader
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)


type Msg
    = GetTemplateCompleted (Result ApiError DocumentTemplateDetail)
    | DropdownMsg Dropdown.State
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | UpdatePhase DocumentTemplatePhase
    | UpdatePhaseCompleted (Result ApiError DocumentTemplateDetail)
    | ExportTemplate DocumentTemplateDetail
    | FileDownloaderMsg FileDownloader.Msg
    | ShowAllKms
