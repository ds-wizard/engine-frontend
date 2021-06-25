module Wizard.Projects.Create.ProjectCreateRoute exposing (ProjectCreateRoute(..))


type ProjectCreateRoute
    = TemplateCreateRoute (Maybe String)
    | CustomCreateRoute (Maybe String)
