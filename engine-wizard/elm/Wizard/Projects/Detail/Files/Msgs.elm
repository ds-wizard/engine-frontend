module Wizard.Projects.Detail.Files.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = ListingMsg (Listing.Msg QuestionnaireFile)
    | DownloadFile QuestionnaireFile
    | FileDownloaderMsg FileDownloader.Msg
    | ShowHideDeleteFile (Maybe QuestionnaireFile)
    | DeleteFileConfirm
    | DeleteFileCompleted (Result ApiError ())
