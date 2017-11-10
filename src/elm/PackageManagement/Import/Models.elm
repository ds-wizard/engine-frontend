module PackageManagement.Import.Models exposing (..)

import FileReader exposing (NativeFile)


type alias Model =
    { error : String
    , dnd : Int
    , files : List NativeFile
    }


initialModel : Model
initialModel =
    { error = ""
    , dnd = 0
    , files = []
    }
