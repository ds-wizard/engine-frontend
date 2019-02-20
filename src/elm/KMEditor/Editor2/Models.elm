module KMEditor.Editor2.Models exposing (Model, initialModel)


type alias Model =
    { branchUuid : String
    }


initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    }
