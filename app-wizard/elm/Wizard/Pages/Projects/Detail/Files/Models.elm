module Wizard.Pages.Projects.Detail.Files.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.ProjectFile exposing (ProjectFile)
import Wizard.Components.Listing.Models as Listing


type alias Model =
    { projectFiles : Listing.Model ProjectFile
    , projectFileToBeDeleted : Maybe ProjectFile
    , deletingProjectFile : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { projectFiles = Listing.initialModel paginationQueryString
    , projectFileToBeDeleted = Nothing
    , deletingProjectFile = ActionResult.Unset
    }
