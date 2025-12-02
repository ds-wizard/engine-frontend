module Wizard.Pages.Projects.CreateMigration.Subscriptions exposing (subscriptions)

import Common.Components.TypeHintInput as TypeHintInput
import Wizard.Pages.Projects.CreateMigration.Models exposing (Model)
import Wizard.Pages.Projects.CreateMigration.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KnowledgeModelPackageTypeHintInputMsg <|
        TypeHintInput.subscriptions model.knowledgeModelPackageTypeHintInputModel
