module Wizard.Projects.Import.Subscriptions exposing (subscriptions)

import ActionResult
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Ports as Ports
import Wizard.Projects.Import.Models exposing (Model)
import Wizard.Projects.Import.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ActionResult.unwrap Sub.none (Sub.map QuestionnaireMsg << Questionnaire.subscriptions) model.questionnaireModel
        , Ports.gotImporterData GotImporterData
        ]
