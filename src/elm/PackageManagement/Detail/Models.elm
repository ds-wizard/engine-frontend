module PackageManagement.Detail.Models exposing (..)

import PackageManagement.Models exposing (PackageDetail)


type alias Model =
    { packages : List PackageDetail
    , loading : Bool
    , error : String
    , showDeleteDialog : Bool
    , deletingPackage : Bool
    , deleteError : String
    }


initialModel : Model
initialModel =
    { packages = []
    , loading = True
    , error = ""
    , showDeleteDialog = False
    , deletingPackage = False
    , deleteError = ""
    }
