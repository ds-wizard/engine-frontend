module Wizard.KMEditor.Editor.Subscriptions exposing (subscriptions)

import Shared.WebSocket as WebSocket
import Wizard.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.KMEditor.Editor.Components.Preview as Preview
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute exposing (KMEditorRoute)
import Wizard.KMEditor.Editor.Models exposing (Model)
import Wizard.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving


subscriptions : KMEditorRoute -> Model -> Sub Msg
subscriptions route model =
    let
        pageSubscriptions =
            case route of
                KMEditorRoute.Edit _ ->
                    Sub.map KMEditorMsg <|
                        KMEditor.subscriptions model.kmEditorModel

                KMEditorRoute.Preview ->
                    Sub.map PreviewMsg <|
                        Preview.subscriptions model.previewModel

                _ ->
                    Sub.none
    in
    Sub.batch
        [ WebSocket.listen WebSocketMsg
        , WebSocket.schedulePing WebSocketPing
        , Sub.map SavingMsg <| ProjectSaving.subscriptions model.savingModel
        , pageSubscriptions
        ]
