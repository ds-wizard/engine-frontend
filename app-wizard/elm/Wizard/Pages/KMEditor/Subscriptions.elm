module Wizard.Pages.KMEditor.Subscriptions exposing (subscriptions)

import Wizard.Msgs
import Wizard.Pages.KMEditor.Create.Subscriptions
import Wizard.Pages.KMEditor.Editor.Subscriptions
import Wizard.Pages.KMEditor.Index.Subscriptions
import Wizard.Pages.KMEditor.Models exposing (Model)
import Wizard.Pages.KMEditor.Msgs exposing (Msg(..))
import Wizard.Pages.KMEditor.Routes exposing (Route(..))


subscriptions : (Msg -> Wizard.Msgs.Msg) -> Route -> Model -> Sub Wizard.Msgs.Msg
subscriptions wrapMsg route model =
    case route of
        CreateRoute _ _ ->
            Sub.map (wrapMsg << CreateMsg) <|
                Wizard.Pages.KMEditor.Create.Subscriptions.subscriptions model.createModel

        EditorRoute _ subroute ->
            Sub.map (wrapMsg << EditorMsg) <|
                Wizard.Pages.KMEditor.Editor.Subscriptions.subscriptions subroute model.editorModel

        IndexRoute _ ->
            Sub.map (wrapMsg << IndexMsg) <|
                Wizard.Pages.KMEditor.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
