module Wizard.Pages.Documents.Index.Msgs exposing (Msg(..))

import Shared.Components.FileDownloader as FileDownloader
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Api.Models.QuestionnaireCommon exposing (QuestionnaireCommon)
import Wizard.Api.Models.Submission exposing (Submission)
import Wizard.Api.Models.SubmissionService exposing (SubmissionService)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireCommon)
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
