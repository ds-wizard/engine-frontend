module Wizard.KnowledgeModels.Import.Subscriptions exposing (subscriptions)

import Wizard.KnowledgeModels.Import.FileImport.Subscriptions as FileImport
import Wizard.KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import Wizard.KnowledgeModels.Import.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Import.OwlImport.Subscriptions as OwlImport


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.importModel of
        FileImportModel fileImportModel ->
            Sub.map FileImportMsg <|
                FileImport.subscriptions fileImportModel

        OwlImportModel _ ->
            Sub.map OwlImportMsg OwlImport.subscriptions

        _ ->
            Sub.none
