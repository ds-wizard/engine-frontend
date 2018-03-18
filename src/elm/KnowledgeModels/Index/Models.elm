module KnowledgeModels.Index.Models exposing (..)

{-|

@docs Model, initialModel

-}

import Common.Form exposing (CustomFormError)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import KnowledgeModels.Models exposing (KnowledgeModel, KnowledgeModelUpgradeForm, initKnowledgeModelUpgradeForm)
import PackageManagement.Models exposing (PackageDetail)


{-| -}
type alias Model =
    { knowledgeModels : ActionResult (List KnowledgeModel)
    , kmToBeDeleted : Maybe KnowledgeModel
    , deletingKnowledgeModel : ActionResult String
    , creatingMigration : ActionResult String
    , kmToBeUpgraded : Maybe KnowledgeModel
    , packages : ActionResult (List PackageDetail)
    , kmUpgradeForm : Form CustomFormError KnowledgeModelUpgradeForm
    , deletingMigration : ActionResult String
    }


{-| -}
initialModel : Model
initialModel =
    { knowledgeModels = Loading
    , kmToBeDeleted = Nothing
    , deletingKnowledgeModel = Unset
    , creatingMigration = Unset
    , kmToBeUpgraded = Nothing
    , packages = Unset
    , kmUpgradeForm = initKnowledgeModelUpgradeForm
    , deletingMigration = Unset
    }
