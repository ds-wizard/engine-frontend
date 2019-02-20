module KMEditor.Editor2.Subscriptions exposing (subscriptions)

import KMEditor.Editor2.Models exposing (Model)
import KMEditor.Editor2.Msgs exposing (Msg)
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    Sub.none
