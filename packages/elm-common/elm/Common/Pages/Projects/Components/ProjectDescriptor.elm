module Common.Pages.Projects.Components.ProjectDescriptor exposing
    ( ProjectDescriptor
    , fromProject
    , fromProjectSettings
    )

{-| What the clone and delete dialogs need to know about a project: which one,
what it is called, and what it is about.
-}

import Common.Api.Models.Project exposing (Project)
import Common.Api.Models.ProjectSettings exposing (ProjectSettings)
import Uuid exposing (Uuid)


type alias ProjectDescriptor =
    { name : String
    , uuid : Uuid
    , description : Maybe String
    }


fromProject : Project -> ProjectDescriptor
fromProject project =
    { name = project.name
    , uuid = project.uuid
    , description = project.description
    }


fromProjectSettings : ProjectSettings -> ProjectDescriptor
fromProjectSettings settings =
    { name = settings.name
    , uuid = settings.uuid
    , description = settings.description
    }
