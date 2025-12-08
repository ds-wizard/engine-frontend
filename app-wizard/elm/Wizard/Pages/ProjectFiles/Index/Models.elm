module Wizard.Pages.ProjectFiles.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.ProjectFile exposing (ProjectFile)
import Wizard.Components.Listing.Models as Listing


type alias Model =
    { questionnaireFiles : Listing.Model ProjectFile
    , questionnaireFileToBeDeleted : Maybe ProjectFile
    , deletingQuestionnaireFile : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaireFiles = Listing.initialModel paginationQueryString
    , questionnaireFileToBeDeleted = Nothing
    , deletingQuestionnaireFile = ActionResult.Unset
    }
