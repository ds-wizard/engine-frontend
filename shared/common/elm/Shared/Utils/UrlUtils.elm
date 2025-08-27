module Shared.Utils.UrlUtils exposing (queryParamsToString)


queryParamsToString : List ( String, Maybe String ) -> String
queryParamsToString params =
    let
        fold ( item, mbValue ) acc =
            case mbValue of
                Just value ->
                    acc ++ [ item ++ "=" ++ value ]

                Nothing ->
                    acc

        paramList =
            List.foldl fold [] params
    in
    if List.isEmpty paramList then
        ""

    else
        "?" ++ String.join "&" paramList
