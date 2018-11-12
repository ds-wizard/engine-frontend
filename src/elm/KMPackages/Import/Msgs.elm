module KMPackages.Import.Msgs exposing (Msg(..))

import Json.Decode
import Jwt
import Ports exposing (FilePortData)


type Msg
    = DragEnter
    | DragOver
    | DragLeave
    | FileSelected
    | FileRead FilePortData
    | Submit
    | Cancel
    | ImportPackageCompleted (Result Jwt.JwtError Json.Decode.Value)
