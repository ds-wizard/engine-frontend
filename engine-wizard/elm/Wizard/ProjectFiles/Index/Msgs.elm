module Wizard.ProjectFiles.Index.Msgs exposing (Msg(..))

import Shared.Data.QuestionnaireFile exposing (QuestionnaireFile)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = ListingMsg (Listing.Msg QuestionnaireFile)
    | DownloadFile QuestionnaireFile
    | FileDownloaderMsg FileDownloader.Msg
    | ShowHideDeleteFile (Maybe QuestionnaireFile)
    | DeleteFileConfirm
    | DeleteFileCompleted (Result ApiError ())
