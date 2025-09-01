module Wizard.DocumentTemplateEditors.Subscriptions exposing (subscriptions)

import Time
import Wizard.DocumentTemplateEditors.Create.Subscriptions
import Wizard.DocumentTemplateEditors.Editor.Subscriptions
import Wizard.DocumentTemplateEditors.Index.Subcriptions
import Wizard.DocumentTemplateEditors.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Msgs exposing (Msg(..))
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))


subscriptions : (Msg -> msg) -> (Time.Posix -> msg) -> Route -> Model -> Sub msg
subscriptions wrapMsg onTime route model =
    case route of
        CreateRoute _ _ ->
            Sub.map (wrapMsg << CreateMsg) <|
                Wizard.DocumentTemplateEditors.Create.Subscriptions.subscriptions model.createModel

        IndexRoute _ ->
            Sub.map (wrapMsg << IndexMsg) <|
                Wizard.DocumentTemplateEditors.Index.Subcriptions.subscriptions model.indexModel

        EditorRoute _ _ ->
            Wizard.DocumentTemplateEditors.Editor.Subscriptions.subscriptions (wrapMsg << EditorMsg) onTime model.editorModel
