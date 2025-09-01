module Wizard.Pages.Projects.Import.Subscriptions exposing (subscriptions)

import ActionResult
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Data.Integrations as Integrations
import Wizard.Pages.Projects.Import.Models exposing (Model)
import Wizard.Pages.Projects.Import.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ActionResult.unwrap Sub.none (Sub.map QuestionnaireMsg << Questionnaire.subscriptions) model.questionnaireModel
        , Integrations.importerSub GotImporterData
        ]
