module KMEditor.Create.Models exposing
    ( Model
    , initialModel
    , setSelectedPackage
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import KMEditor.Common.BranchCreateForm as BranchCreateForm exposing (BranchCreateForm)
import KnowledgeModels.Common.Package exposing (Package)


type alias Model =
    { packages : ActionResult (List Package)
    , savingBranch : ActionResult ()
    , form : Form CustomFormError BranchCreateForm
    , selectedPackage : Maybe String
    }


initialModel : Maybe String -> Model
initialModel selectedPackage =
    { packages = Loading
    , savingBranch = Unset
    , form = BranchCreateForm.init selectedPackage
    , selectedPackage = selectedPackage
    }


setSelectedPackage : Model -> List Package -> Model
setSelectedPackage model packages =
    case model.selectedPackage of
        Just id ->
            if List.any (.id >> (==) id) packages then
                { model | form = BranchCreateForm.init model.selectedPackage }

            else
                model

        _ ->
            model
