module Shared.RegexPatterns exposing (email, kmId, organizationId, projectTag, url, uuid)

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


fromString : String -> Regex
fromString =
    Maybe.withDefault Regex.never << Regex.fromStringWith { caseInsensitive = False, multiline = False }


fromStringIC : String -> Regex
fromStringIC =
    Maybe.withDefault Regex.never << Regex.fromStringWith { caseInsensitive = False, multiline = False }
