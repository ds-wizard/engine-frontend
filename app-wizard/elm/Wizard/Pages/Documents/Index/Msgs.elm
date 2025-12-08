module Wizard.Pages.Documents.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Api.Models.ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.Submission exposing (Submission)
import Wizard.Api.Models.SubmissionService exposing (SubmissionService)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = GetQuestionnaireCompleted (Result ApiError ProjectCommon)
    | ShowHideDeleteDocument (Maybe Document)
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
