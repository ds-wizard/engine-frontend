module KnowledgeModels.Detail.Models exposing (Model, PackageDetailRow, currentPackage, initPackageDetailRow, initialModel, packagesLength, sortPackageDetailRowsByVersion)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import KnowledgeModels.Common.Models exposing (PackageDetail)
import Utils exposing (versionIsGreater)


type alias Model =
    { packages : ActionResult (List PackageDetailRow)
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    , versionToBeDeleted : Maybe String
    }


initialModel : Model
initialModel =
    { packages = Loading
    , deletingVersion = Unset
    , showDeleteDialog = False
    , versionToBeDeleted = Nothing
    }


type alias PackageDetailRow =
    { dropdownState : Dropdown.State
    , packageDetail : PackageDetail
    }


initPackageDetailRow : PackageDetail -> PackageDetailRow
initPackageDetailRow =
    PackageDetailRow Dropdown.initialState


currentPackage : Model -> Maybe PackageDetail
currentPackage model =
    case model.packages of
        Success packages ->
            List.head packages |> Maybe.map .packageDetail

        _ ->
            Nothing


packagesLength : Model -> Int
packagesLength model =
    case model.packages of
        Success packages ->
            List.length packages

        _ ->
            0


sortPackageDetailRowsByVersion : List PackageDetailRow -> List PackageDetailRow
sortPackageDetailRowsByVersion =
    let
        versionSort packageDetailRow1 packageDetailRow2 =
            if versionIsGreater packageDetailRow2.packageDetail.version packageDetailRow1.packageDetail.version then
                LT

            else
                GT
    in
    List.sortWith versionSort
