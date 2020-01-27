module Wizard.KMEditor.Subscriptions exposing (subscriptions)

import Wizard.KMEditor.Editor.Subscriptions
import Wizard.KMEditor.Index.Subscriptions
import Wizard.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Msgs


subscriptions : (Msg -> Wizard.Msgs.Msg) -> Route -> Model -> Sub Wizard.Msgs.Msg
subscriptions wrapMsg route model =
    case route of
        EditorRoute _ ->
            Wizard.KMEditor.Editor.Subscriptions.subscriptions (wrapMsg << EditorMsg) model.editorModel

        IndexRoute ->
            Sub.map (wrapMsg << IndexMsg) <|
                Wizard.KMEditor.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
