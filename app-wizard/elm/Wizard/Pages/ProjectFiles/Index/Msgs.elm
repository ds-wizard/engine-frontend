module Wizard.Pages.ProjectFiles.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.ProjectFile exposing (ProjectFile)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg ProjectFile)
    | DownloadFile ProjectFile
    | FileDownloaderMsg FileDownloader.Msg
    | ShowHideDeleteFile (Maybe ProjectFile)
    | DeleteFileConfirm
    | DeleteFileCompleted (Result ApiError ())
