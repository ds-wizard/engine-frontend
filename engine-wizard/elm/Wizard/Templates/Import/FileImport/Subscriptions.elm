module Wizard.Templates.Import.FileImport.Subscriptions exposing (subscriptions)

import Wizard.Ports as Ports
import Wizard.Templates.Import.FileImport.Models exposing (Model)
import Wizard.Templates.Import.FileImport.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.fileContentRead FileRead
