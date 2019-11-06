module Wizard.KnowledgeModels.Import.Subscriptions exposing (subscriptions)

import Wizard.KnowledgeModels.Import.FileImport.Subscriptions as FileImport
import Wizard.KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import Wizard.KnowledgeModels.Import.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.importModel of
        FileImportModel fileImportModel ->
            Sub.map FileImportMsg <|
                FileImport.subscriptions fileImportModel

        _ ->
            Sub.none
