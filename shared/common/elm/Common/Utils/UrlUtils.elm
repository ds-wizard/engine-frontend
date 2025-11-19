module Common.Utils.UrlUtils exposing
    ( addOptionalUrlPart
    , addQueryParam
    , getDomain
    , queryParamsToString
    )

import Url exposing (Url)


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


addQueryParam : String -> String -> Url -> Url
addQueryParam paramName paramValue url =
    let
        paramString =
            paramName ++ "=" ++ paramValue

        newQuery =
            case url.query of
                Just query ->
                    Just (query ++ "&" ++ paramString)

                Nothing ->
                    Just paramString
    in
    { url | query = newQuery }


addOptionalUrlPart : Maybe String -> List String -> List String
addOptionalUrlPart maybePart parts =
    case maybePart of
        Just part ->
            parts ++ [ part ]

        Nothing ->
            parts


getDomain : String -> Maybe String
getDomain urlString =
    Maybe.map .host (Url.fromString urlString)
