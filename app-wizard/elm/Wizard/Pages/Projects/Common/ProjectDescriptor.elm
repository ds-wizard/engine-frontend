module Wizard.Pages.Projects.Common.ProjectDescriptor exposing
    ( ProjectDescriptor
    , fromProject
    , fromProjectSettings
    )

import Uuid exposing (Uuid)
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Api.Models.ProjectSettings exposing (ProjectSettings)


type alias ProjectDescriptor =
    { name : String
    , uuid : Uuid
    }


fromProject : Project -> ProjectDescriptor
fromProject project =
    { name = project.name
    , uuid = project.uuid
    }


fromProjectSettings : ProjectSettings -> ProjectDescriptor
fromProjectSettings settings =
    { name = settings.name
    , uuid = settings.uuid
    }
