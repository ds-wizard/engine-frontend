module Wizard.Pages.KnowledgeModels.Compare.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Components.KMComparison as KMComparison
import Wizard.Pages.KnowledgeModels.Common.CompareSelectModal as CompareSelectModal


type alias Model =
    { initialLoading : ActionResult ()
    , kmComparisonModel : KMComparison.Model
    , compareSelectModalModel : CompareSelectModal.Model
    }


initialModel : Maybe Uuid -> Model
initialModel mbLeftKmUuid =
    let
        initialLoading =
            if Maybe.isJust mbLeftKmUuid then
                ActionResult.Loading

            else
                ActionResult.Success ()
    in
    { initialLoading = initialLoading
    , kmComparisonModel = KMComparison.initialModel
    , compareSelectModalModel = CompareSelectModal.initialModel
    }
