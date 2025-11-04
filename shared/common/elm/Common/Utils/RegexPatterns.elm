module Common.Utils.RegexPatterns exposing
    ( authenticationServiceId
    , color
    , date
    , datetime
    , documentTemplateId
    , doi
    , email
    , escapeRegex
    , fromString
    , fromStringIC
    , jinjaSafe
    , kmId
    , kmSecret
    , localeId
    , orcid
    , organizationId
    , projectTag
    , time
    , url
    , uuid
    )

import Regex exposing (Regex)


email : Regex
email =
    fromStringIC "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"


uuid : Regex
uuid =
    fromStringIC "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"


url : Regex
url =
    fromString "^(?:(?:https?|ftp):\\/\\/)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,})))(?::\\d{2,5})?(?:\\/\\S*)?$"


orcid : Regex
orcid =
    fromString "^(\\d{4}-){3}\\d{3}(\\d|X)$"


doi : Regex
doi =
    fromStringIC "^10\\.\\d{4,9}\\/[-._;()\\/:A-Z0-9]+$"


organizationId : Regex
organizationId =
    fromString "^[A-Za-z0-9-_.]+$"


kmId : Regex
kmId =
    fromString "^[A-Za-z0-9-_.]+$"


kmSecret : Regex
kmSecret =
    fromString "^[A-Za-z0-9-_.]+$"


documentTemplateId : Regex
documentTemplateId =
    fromString "^[A-Za-z0-9-_.]+$"


localeId : Regex
localeId =
    fromString "^[A-Za-z0-9-_.]+$"


authenticationServiceId : Regex
authenticationServiceId =
    fromString "^[a-z0-9-]+$"


projectTag : Regex
projectTag =
    fromString "^[^,]+$"


datetime : Regex
datetime =
    fromString "^[0-9]{4}-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])) (([01][0-9])|2[0-3]):[0-5][0-9]$"


date : Regex
date =
    fromString "^[0-9]{4}-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01]))$"


time : Regex
time =
    fromString "^(([01][1-9])|2[0-3]):[0-5][0-9]$"


color : Regex
color =
    fromString "^#(?:[0-9a-fA-F]{3}){1,2}$"


jinjaSafe : Regex
jinjaSafe =
    fromStringIC "^[a-z_][a-z0-9_]*$"


fromString : String -> Regex
fromString =
    Maybe.withDefault Regex.never << Regex.fromStringWith { caseInsensitive = False, multiline = False }


fromStringIC : String -> Regex
fromStringIC =
    Maybe.withDefault Regex.never << Regex.fromStringWith { caseInsensitive = True, multiline = False }


escapeRegex : String -> String
escapeRegex str =
    let
        specials =
            [ "\\", ".", "+", "*", "?", "^", "$", "(", ")", "[", "]", "{", "}", "|" ]

        escapeChar c =
            if List.member (String.fromChar c) specials then
                "\\" ++ String.fromChar c

            else
                String.fromChar c
    in
    String.concat (List.map escapeChar (String.toList str))
