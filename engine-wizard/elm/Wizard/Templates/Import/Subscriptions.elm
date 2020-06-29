module Wizard.Templates.Import.Subscriptions exposing (subscriptions)

import Wizard.Templates.Import.FileImport.Subscriptions as FileImport
import Wizard.Templates.Import.Models exposing (ImportModel(..), Model)
import Wizard.Templates.Import.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.importModel of
        FileImportModel fileImportModel ->
            Sub.map FileImportMsg <|
                FileImport.subscriptions fileImportModel

        _ ->
            Sub.none
