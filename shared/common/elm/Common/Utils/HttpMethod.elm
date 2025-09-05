module Common.Utils.HttpMethod exposing (options)

import List.Extra as List


options : List ( String, String )
options =
    let
        httpMethods =
            [ "GET", "POST", "HEAD", "PUT", "DELETE", "OPTIONS", "PATCH" ]
    in
    ( "", "--" ) :: List.zip httpMethods httpMethods
