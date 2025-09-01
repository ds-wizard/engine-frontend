module Wizard.Projects.Create.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.Create.Models exposing (Model)
import Wizard.Projects.Create.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ProjectTemplateTypeHintInputMsg <|
            TypeHintInput.subscriptions model.projectTemplateTypeHintInputModel
        , Sub.map KnowledgeModelTypeHintInputMsg <|
            TypeHintInput.subscriptions model.knowledgeModelTypeHintInputModel
        ]
