module Wizard.Pages.Projects.Create.Subscriptions exposing (subscriptions)

import Common.Components.TypeHintInput as TypeHintInput
import Wizard.Pages.Projects.Create.Models exposing (Model)
import Wizard.Pages.Projects.Create.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ProjectTemplateTypeHintInputMsg <|
            TypeHintInput.subscriptions model.projectTemplateTypeHintInputModel
        , Sub.map KnowledgeModelTypeHintInputMsg <|
            TypeHintInput.subscriptions model.knowledgeModelTypeHintInputModel
        ]
