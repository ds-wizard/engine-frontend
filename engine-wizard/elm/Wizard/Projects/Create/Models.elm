module Wizard.Projects.Create.Models exposing
    ( CreateModel(..)
    , Model
    , empty
    , initialModel
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Create.CustomCreate.Models as CustomCreateModels
import Wizard.Projects.Create.ProjectCreateRoute as ProjectCreateRoute exposing (ProjectCreateRoute)
import Wizard.Projects.Create.TemplateCreate.Models as TemplateCreateModels


type CreateModel
    = TemplateCreateModel TemplateCreateModels.Model
    | CustomCreateModel CustomCreateModels.Model


type alias Model =
    { createModel : CreateModel }


empty : Model
empty =
    { createModel = TemplateCreateModel (TemplateCreateModels.initialModel Nothing) }


initialModel : AppState -> ProjectCreateRoute -> Model
initialModel appState subroute =
    case subroute of
        ProjectCreateRoute.TemplateCreateRoute mbSelected ->
            { createModel = TemplateCreateModel (TemplateCreateModels.initialModel mbSelected) }

        ProjectCreateRoute.CustomCreateRoute mbSelected ->
            { createModel = CustomCreateModel (CustomCreateModels.initialModel appState mbSelected) }
