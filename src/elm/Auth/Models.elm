module Auth.Models exposing (..)

import Json.Encode as Encode exposing (..)


type alias Model =
    { email : String
    , password : String
    , token : String
    , error : String
    }


initialModel : String -> Model
initialModel token =
    { email = ""
    , password = ""
    , token = token
    , error = ""
    }
