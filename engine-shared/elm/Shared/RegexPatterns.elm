module Shared.RegexPatterns exposing (color, date, datetime, email, kmId, organizationId, projectTag, time, url, uuid)

import Regex exposing (Regex)


email : Regex
email =
    fromStringIC "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"


uuid : Regex
uuid =
    fromStringIC "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"


url : Regex
url =
    fromString "^(ftp|http|https):\\/\\/(?:www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b(?:[-a-zA-Z0-9()@:%_\\+.~#?&\\/=]*)$"


organizationId : Regex
organizationId =
    fromString "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"


kmId : Regex
kmId =
    fromString "^^(?![-])(?!.*[-]$)[a-zA-Z0-9-]+$"


projectTag : Regex
projectTag =
    fromString "^[^,]+$"


datetime : Regex
datetime =
    fromString "^[0-9]{4}-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])) (([01][1-9])|2[0-3]):[0-5][0-9]$"


date : Regex
date =
    fromString "^[0-9]{4}-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01]))$"


time : Regex
time =
    fromString "^(([01][1-9])|2[0-3]):[0-5][0-9]$"


color : Regex
color =
    fromString "^#(?:[0-9a-fA-F]{3}){1,2}$"


fromString : String -> Regex
fromString =
    Maybe.withDefault Regex.never << Regex.fromStringWith { caseInsensitive = False, multiline = False }


fromStringIC : String -> Regex
fromStringIC =
    Maybe.withDefault Regex.never << Regex.fromStringWith { caseInsensitive = False, multiline = False }
