module Wizard.Locales.Import.FileImport.Subscriptions exposing (subscriptions)

import Wizard.Locales.Import.FileImport.Models exposing (Model)
import Wizard.Locales.Import.FileImport.Msgs exposing (Msg(..))
import Wizard.Ports as Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.fileContentRead FileRead
