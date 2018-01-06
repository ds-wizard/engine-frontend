module KnowledgeModels.Publish.Models exposing (..)

{-|

@docs Model, initialModel

-}

import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import KnowledgeModels.Models exposing (..)


{-| -}
type alias Model =
    { knowledgeModel : ActionResult KnowledgeModel
    , publishingKnowledgeModel : ActionResult String
    , form : Form () KnowledgeModelPublishForm
    }


{-| -}
initialModel : Model
initialModel =
    { knowledgeModel = Loading
    , publishingKnowledgeModel = Unset
    , form = initKnowledgeModelPublishForm
    }
