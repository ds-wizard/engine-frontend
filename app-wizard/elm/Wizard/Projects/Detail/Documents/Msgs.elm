module Wizard.Projects.Detail.Documents.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Api.Models.Submission exposing (Submission)
import Wizard.Api.Models.SubmissionService exposing (SubmissionService)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = ShowHideDeleteDocument (Maybe Document)
    | DeleteDocument
    | DeleteDocumentCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Document)
    | ShowHideSubmitDocument (Maybe Document)
    | GetSubmissionServicesCompleted (Result ApiError (List SubmissionService))
    | SelectSubmissionService String
    | SubmitDocument
    | SubmitDocumentCompleted (Result ApiError Submission)
    | SetDocumentErrorModal (Maybe String)
    | SetSubmissionErrorModal (Maybe String)
    | DownloadDocument Document
    | FileDownloaderMsg FileDownloader.Msg
