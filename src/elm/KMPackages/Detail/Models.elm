module KMPackages.Detail.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import KMPackages.Models exposing (PackageDetail)


type alias Model =
    { packages : ActionResult (List PackageDetail)
    , deletingPackage : ActionResult String
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    , versionToBeDeleted : Maybe String
    }


initialModel : Model
initialModel =
    { packages = Loading
    , deletingPackage = Unset
    , deletingVersion = Unset
    , showDeleteDialog = False
    , versionToBeDeleted = Nothing
    }


currentPackage : Model -> Maybe PackageDetail
currentPackage model =
    case model.packages of
        Success packages ->
            List.head packages

        _ ->
            Nothing


packagesLength : Model -> Int
packagesLength model =
    case model.packages of
        Success packages ->
            List.length packages

        _ ->
            0
