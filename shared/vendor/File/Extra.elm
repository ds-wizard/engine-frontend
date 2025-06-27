module File.Extra exposing (ext)

import File exposing (File)
import List.Extra as List


ext : File -> Maybe String
ext =
    List.last << String.split "." << File.name
