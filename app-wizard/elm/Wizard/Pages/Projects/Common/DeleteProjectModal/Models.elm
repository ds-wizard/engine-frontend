module Wizard.Pages.Projects.Common.DeleteProjectModal.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Pages.Projects.Common.ProjectDescriptor exposing (ProjectDescriptor)


type alias Model =
    { projectToBeDeleted : Maybe ProjectDescriptor
    , deletingProject : ActionResult String
    }


initialModel : Model
initialModel =
    { projectToBeDeleted = Nothing
    , deletingProject = Unset
    }
