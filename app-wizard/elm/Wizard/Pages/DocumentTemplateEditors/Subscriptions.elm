module Wizard.Pages.DocumentTemplateEditors.Subscriptions exposing (subscriptions)

import Time
import Wizard.Pages.DocumentTemplateEditors.Create.Subscriptions
import Wizard.Pages.DocumentTemplateEditors.Editor.Subscriptions
import Wizard.Pages.DocumentTemplateEditors.Index.Subcriptions
import Wizard.Pages.DocumentTemplateEditors.Models exposing (Model)
import Wizard.Pages.DocumentTemplateEditors.Msgs exposing (Msg(..))
import Wizard.Pages.DocumentTemplateEditors.Routes exposing (Route(..))


subscriptions : (Msg -> msg) -> (Time.Posix -> msg) -> Route -> Model -> Sub msg
subscriptions wrapMsg onTime route model =
    case route of
        CreateRoute _ _ ->
            Sub.map (wrapMsg << CreateMsg) <|
                Wizard.Pages.DocumentTemplateEditors.Create.Subscriptions.subscriptions model.createModel

        IndexRoute _ ->
            Sub.map (wrapMsg << IndexMsg) <|
                Wizard.Pages.DocumentTemplateEditors.Index.Subcriptions.subscriptions model.indexModel

        EditorRoute _ _ ->
            Wizard.Pages.DocumentTemplateEditors.Editor.Subscriptions.subscriptions (wrapMsg << EditorMsg) onTime model.editorModel
