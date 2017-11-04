module UserManagement.Create.Models exposing (..)

import Form exposing (Form)
import Random.Pcg exposing (Seed, initialSeed)
import UserManagement.Models exposing (UserCreateForm, initUserCreateForm)


type alias Model =
    { form : Form () UserCreateForm
    , currentSeed : Seed
    , savingUser : Bool
    , error : String
    }


initialModel : Int -> Model
initialModel seed =
    { form = initUserCreateForm
    , currentSeed = initialSeed seed
    , savingUser = False
    , error = ""
    }
