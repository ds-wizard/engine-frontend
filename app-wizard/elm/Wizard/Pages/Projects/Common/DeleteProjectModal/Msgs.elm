module Wizard.Pages.Projects.Common.DeleteProjectModal.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Pages.Projects.Common.ProjectDescriptor exposing (ProjectDescriptor)


type Msg
    = ShowHideDeleteProject (Maybe ProjectDescriptor)
    | DeleteProject
    | DeleteProjectCompleted (Result ApiError ())
