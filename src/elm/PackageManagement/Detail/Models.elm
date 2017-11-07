module PackageManagement.Detail.Models exposing (..)

import PackageManagement.Models exposing (PackageDetail)


type alias Model =
    { package : Maybe PackageDetail
    , loading : Bool
    , error : String
    }


initialModel : Model
initialModel =
    { package = Nothing
    , loading = True
    , error = ""
    }
