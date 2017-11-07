module PackageManagement.Index.Models exposing (..)

import PackageManagement.Models exposing (Package)


type alias Model =
    { packages : List Package
    , loading : Bool
    , error : String
    }


initialModel : Model
initialModel =
    { packages = []
    , loading = True
    , error = ""
    }
