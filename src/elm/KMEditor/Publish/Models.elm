module KMEditor.Publish.Models exposing (..)

{-|

@docs Model, initialModel

-}

import Common.Form exposing (CustomFormError)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import KMEditor.Models exposing (..)


{-| -}
type alias Model =
    { knowledgeModel : ActionResult KnowledgeModel
    , publishingKnowledgeModel : ActionResult String
    , form : Form CustomFormError KnowledgeModelPublishForm
    }


{-| -}
initialModel : Model
initialModel =
    { knowledgeModel = Loading
    , publishingKnowledgeModel = Unset
    , form = initKnowledgeModelPublishForm
    }
