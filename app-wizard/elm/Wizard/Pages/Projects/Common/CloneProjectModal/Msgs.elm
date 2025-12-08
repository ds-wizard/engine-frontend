module Wizard.Pages.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Pages.Projects.Common.ProjectDescriptor exposing (ProjectDescriptor)


type Msg
    = ShowHideCloneProject (Maybe ProjectDescriptor)
    | CloneProject
    | CloneProjectCompleted (Result ApiError Project)
