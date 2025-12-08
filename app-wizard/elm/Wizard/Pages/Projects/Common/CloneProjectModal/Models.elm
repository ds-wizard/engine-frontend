module Wizard.Pages.Projects.Common.CloneProjectModal.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Pages.Projects.Common.ProjectDescriptor exposing (ProjectDescriptor)


type alias Model =
    { projectToBeDeleted : Maybe ProjectDescriptor
    , cloningProject : ActionResult String
    }


initialModel : Model
initialModel =
    { projectToBeDeleted = Nothing
    , cloningProject = Unset
    }
