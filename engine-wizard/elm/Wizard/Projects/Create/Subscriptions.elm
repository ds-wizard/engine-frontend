module Wizard.Projects.Create.Subscriptions exposing (subscriptions)

import Wizard.Projects.Create.CustomCreate.Subscriptions as CustomCreate
import Wizard.Projects.Create.Models exposing (CreateModel(..), Model)
import Wizard.Projects.Create.Msgs exposing (Msg(..))
import Wizard.Projects.Create.TemplateCreate.Subscriptions as TemplateCreate


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.createModel of
        CustomCreateModel customCreateModel ->
            Sub.map CustomCreateMsg <|
                CustomCreate.subscriptions customCreateModel

        TemplateCreateModel templateCreateModel ->
            Sub.map TemplateCreateMsg <|
                TemplateCreate.subscriptions templateCreateModel
