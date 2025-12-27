module Wizard.Pages.Projects.ImportLegacy.Subscriptions exposing (subscriptions)

import ActionResult
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Data.Integrations as Integrations
import Wizard.Pages.Projects.ImportLegacy.Models exposing (Model)
import Wizard.Pages.Projects.ImportLegacy.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ActionResult.unwrap Sub.none (Sub.map QuestionnaireMsg << Questionnaire.subscriptions) model.questionnaireModel
        , Integrations.importerSub GotImporterData
        ]
