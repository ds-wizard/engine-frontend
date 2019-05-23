module KnowledgeModels.Import.Subscriptions exposing (subscriptions)

import KnowledgeModels.Import.FileImport.Subscriptions as FileImport
import KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import KnowledgeModels.Import.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.importModel of
        FileImportModel fileImportModel ->
            Sub.map FileImportMsg <|
                FileImport.subscriptions fileImportModel

        _ ->
            Sub.none
