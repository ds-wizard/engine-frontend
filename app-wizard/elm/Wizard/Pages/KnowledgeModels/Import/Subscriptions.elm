module Wizard.Pages.KnowledgeModels.Import.Subscriptions exposing (subscriptions)

import Wizard.Pages.KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import Wizard.Pages.KnowledgeModels.Import.Msgs exposing (Msg(..))
import Wizard.Pages.KnowledgeModels.Import.OwlImport.Subscriptions as OwlImport


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.importModel of
        OwlImportModel _ ->
            Sub.map OwlImportMsg OwlImport.subscriptions

        _ ->
            Sub.none
