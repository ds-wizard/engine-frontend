module Wizard.DocumentTemplates.Import.Subscriptions exposing (subscriptions)

import Wizard.DocumentTemplates.Import.FileImport.Subscriptions as FileImport
import Wizard.DocumentTemplates.Import.Models exposing (ImportModel(..), Model)
import Wizard.DocumentTemplates.Import.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.importModel of
        FileImportModel fileImportModel ->
            Sub.map FileImportMsg <|
                FileImport.subscriptions fileImportModel

        _ ->
            Sub.none
