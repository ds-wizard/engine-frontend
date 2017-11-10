module KnowledgeModels.Index.Models exposing (..)

import KnowledgeModels.Models exposing (KnowledgeModel)


type alias Model =
    { knowledgeModels : List KnowledgeModel
    , loading : Bool
    , error : String
    }


initialModel : Model
initialModel =
    { knowledgeModels = []
    , loading = True
    , error = ""
    }
