module Wizard.Pages.Projects.Detail.Files.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Components.Listing.Models as Listing


type alias Model =
    { questionnaireFiles : Listing.Model QuestionnaireFile
    , questionnaireFileToBeDeleted : Maybe QuestionnaireFile
    , deletingQuestionnaireFile : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaireFiles = Listing.initialModel paginationQueryString
    , questionnaireFileToBeDeleted = Nothing
    , deletingQuestionnaireFile = ActionResult.Unset
    }
