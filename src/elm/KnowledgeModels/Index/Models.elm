module KnowledgeModels.Index.Models exposing (..)

import KnowledgeModels.Models exposing (KnowledgeModel)


type alias Model =
    { knowledgeModels : List KnowledgeModel
    , loading : Bool
    , error : String
    , kmToBeDeleted : Maybe KnowledgeModel
    , deletingKM : Bool
    , deleteKMError : String
    }


initialModel : Model
initialModel =
    { knowledgeModels = []
    , loading = True
    , error = ""
    , kmToBeDeleted = Nothing
    , deletingKM = False
    , deleteKMError = ""
    }
