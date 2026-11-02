module Common.Utils.FileUtils exposing
    ( getExtension
    , matchExtension
    )

import File exposing (File)
import Flip exposing (flip)
import List.Extra as List


getExtension : String -> String
getExtension fileName =
    String.split "." fileName
        |> List.last
        |> Maybe.withDefault ""


matchExtension : List String -> File -> Bool
matchExtension extensions file =
    File.name file
        |> getExtension
        |> flip List.member extensions
