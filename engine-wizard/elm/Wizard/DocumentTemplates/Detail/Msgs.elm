module Wizard.DocumentTemplates.Detail.Msgs exposing (Msg(..))

import Shared.Data.DocumentTemplate.DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Shared.Data.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = GetTemplateCompleted (Result ApiError DocumentTemplateDetail)
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | UpdatePhase DocumentTemplatePhase
    | UpdatePhaseCompleted (Result ApiError DocumentTemplateDetail)
    | ExportTemplate DocumentTemplateDetail
    | FileDownloaderMsg FileDownloader.Msg
