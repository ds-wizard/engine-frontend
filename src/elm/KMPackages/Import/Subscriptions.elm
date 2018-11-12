module KMPackages.Import.Subscriptions exposing (subscriptions)

import KMPackages.Import.Models exposing (Model)
import KMPackages.Import.Msgs exposing (Msg(..))
import Msgs
import Ports exposing (fileContentRead)


subscriptions : (Msg -> Msgs.Msg) -> Model -> Sub Msgs.Msg
subscriptions wrapMsg model =
    fileContentRead (wrapMsg << FileRead)
