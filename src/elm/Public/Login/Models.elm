module Public.Login.Models exposing (..)


type alias Model =
    { email : String
    , password : String
    , error : String
    , loading : Bool
    }


initialModel : Model
initialModel =
    { email = ""
    , password = ""
    , error = ""
    , loading = False
    }
