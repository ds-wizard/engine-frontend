module Wizard.Pages.Projects.Detail.Files.Msgs exposing (Msg(..))

import Shared.Components.FileDownloader as FileDownloader
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg QuestionnaireFile)
    | DownloadFile QuestionnaireFile
    | FileDownloaderMsg FileDownloader.Msg
    | ShowHideDeleteFile (Maybe QuestionnaireFile)
    | DeleteFileConfirm
    | DeleteFileCompleted (Result ApiError ())
