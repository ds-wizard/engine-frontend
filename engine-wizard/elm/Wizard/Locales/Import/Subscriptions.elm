module Wizard.Locales.Import.Subscriptions exposing (subscriptions)

import Wizard.Locales.Import.FileImport.Subscriptions as FileImport
import Wizard.Locales.Import.Models exposing (ImportModel(..), Model)
import Wizard.Locales.Import.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.importModel of
        FileImportModel fileImportModel ->
            Sub.map FileImportMsg <|
                FileImport.subscriptions fileImportModel

        _ ->
            Sub.none
