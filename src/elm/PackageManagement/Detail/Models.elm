module PackageManagement.Detail.Models exposing (..)

import PackageManagement.Models exposing (PackageDetail)


type alias Model =
    { packages : List PackageDetail
    , loading : Bool
    , error : String
    }


initialModel : Model
initialModel =
    { packages = []
    , loading = True
    , error = ""
    }
