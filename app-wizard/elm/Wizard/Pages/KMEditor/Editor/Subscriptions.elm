module Wizard.Pages.KMEditor.Editor.Subscriptions exposing (subscriptions)

import Shared.Api.WebSocket as WebSocket
import Wizard.Pages.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.Pages.KMEditor.Editor.Components.Preview as Preview
import Wizard.Pages.KMEditor.Editor.KMEditorRoute as KMEditorRoute exposing (KMEditorRoute)
import Wizard.Pages.KMEditor.Editor.Models exposing (Model)
import Wizard.Pages.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Detail.Components.ProjectSaving as ProjectSaving


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
