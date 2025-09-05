module Wizard.Pages.ProjectFiles.Index.Msgs exposing (Msg(..))

import Common.Components.FileDownloader as FileDownloader
import Common.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg QuestionnaireFile)
    | DownloadFile QuestionnaireFile
    | FileDownloaderMsg FileDownloader.Msg
    | ShowHideDeleteFile (Maybe QuestionnaireFile)
    | DeleteFileConfirm
    | DeleteFileCompleted (Result ApiError ())
