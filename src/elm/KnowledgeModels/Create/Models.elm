module KnowledgeModels.Create.Models exposing (..)

import Form exposing (Form)
import KnowledgeModels.Models exposing (KnowledgeModelCreateForm, initKnowledgeModelCreateForm)
import PackageManagement.Models exposing (PackageDetail)


type alias Model =
    { form : Form () KnowledgeModelCreateForm
    , savingKm : Bool
    , error : String
    , loading : Bool
    , loadingError : String
    , packages : List PackageDetail
    }


initialModel : Model
initialModel =
    { form = initKnowledgeModelCreateForm
    , savingKm = False
    , error = ""
    , loading = True
    , loadingError = ""
    , packages = []
    }
