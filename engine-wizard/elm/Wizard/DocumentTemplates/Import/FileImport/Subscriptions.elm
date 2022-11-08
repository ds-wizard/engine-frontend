module Wizard.DocumentTemplates.Import.FileImport.Subscriptions exposing (subscriptions)

import Wizard.DocumentTemplates.Import.FileImport.Models exposing (Model)
import Wizard.DocumentTemplates.Import.FileImport.Msgs exposing (Msg(..))
import Wizard.Ports as Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.fileContentRead FileRead
