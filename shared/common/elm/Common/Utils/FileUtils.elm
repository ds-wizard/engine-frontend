module Common.Utils.FileUtils exposing (getExtension)

import List.Extra as List


getExtension : String -> String
getExtension fileName =
    String.split "." fileName
        |> List.last
        |> Maybe.withDefault ""
