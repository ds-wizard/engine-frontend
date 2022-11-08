module Wizard.DocumentTemplateEditors.Subscriptions exposing (subscriptions)

import Wizard.DocumentTemplateEditors.Create.Subscriptions
import Wizard.DocumentTemplateEditors.Editor.Subscriptions
import Wizard.DocumentTemplateEditors.Index.Subcriptions
import Wizard.DocumentTemplateEditors.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Msgs exposing (Msg(..))
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        CreateRoute _ _ ->
            Sub.map CreateMsg <|
                Wizard.DocumentTemplateEditors.Create.Subscriptions.subscriptions model.createModel

        IndexRoute _ ->
            Sub.map IndexMsg <|
                Wizard.DocumentTemplateEditors.Index.Subcriptions.subscriptions model.indexModel

        EditorRoute _ ->
            Sub.map EditorMsg <| Wizard.DocumentTemplateEditors.Editor.Subscriptions.subscriptions model.editorModel
