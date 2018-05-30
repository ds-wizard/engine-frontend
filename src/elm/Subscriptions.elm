module Subscriptions exposing (..)

import KMEditor.Subscriptions
import Models exposing (Model)
import Msgs exposing (Msg(KMEditorMsg))
import Routing exposing (Route(KMEditor))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.route of
        KMEditor route ->
            KMEditor.Subscriptions.subscriptions KMEditorMsg route model.kmEditorModel

        _ ->
            Sub.none
