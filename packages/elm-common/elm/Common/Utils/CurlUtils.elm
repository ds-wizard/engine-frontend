module Common.Utils.CurlUtils exposing
    ( Request
    , parseCurl
    )

{-| Reads a cURL command, as copied from a browser's developer tools or an
API's documentation, into the parts of a request: method, URL, headers and
body.

The command is split into words as a shell would split it: single quotes,
double quotes with their escapes, `$'…'` (which browsers use for bodies with
special characters), backslashes and line continuations. The options are then
read as curl reads them: short and long forms, a short option's value joined
to it (`-XPOST`), and options without a value never taking the next word.

-}


type alias Request =
    { method : String
    , url : String
    , headers : List ( String, String )
    , body : String
    }


parseCurl : String -> Request
parseCurl command =
    let
        words =
            case tokenize command of
                "curl" :: rest ->
                    rest

                other ->
                    other
    in
    toRequest (readOptions words emptyCommand)



-- Reading the options


type alias Command =
    { method : Maybe String
    , url : Maybe String
    , headers : List ( String, String )
    , data : List String
    , json : Bool
    , get : Bool
    , head : Bool
    }


emptyCommand : Command
emptyCommand =
    { method = Nothing
    , url = Nothing
    , headers = []
    , data = []
    , json = False
    , get = False
    , head = False
    }


readOptions : List String -> Command -> Command
readOptions words command =
    case words of
        [] ->
            command

        word :: rest ->
            if String.startsWith "--" word && String.length word > 2 then
                readLong (String.dropLeft 2 word) rest command

            else if String.startsWith "-" word && String.length word > 1 then
                readShort (String.toList (String.dropLeft 1 word)) rest command

            else
                readOptions rest (setUrl word command)


readLong : String -> List String -> Command -> Command
readLong name rest command =
    if takesValue name then
        case rest of
            value :: afterValue ->
                readOptions afterValue (withOption name value command)

            [] ->
                command

    else
        readOptions rest (withFlag name command)


{-| Short options can be joined (`-sL`); the first one that takes a value
takes the rest of the word as it, or the next word if nothing is left.
-}
readShort : List Char -> List String -> Command -> Command
readShort letters rest command =
    case letters of
        [] ->
            readOptions rest command

        letter :: moreLetters ->
            let
                name =
                    longName letter
            in
            if takesValue name then
                if List.isEmpty moreLetters then
                    readLong name rest command

                else
                    readOptions rest (withOption name (String.fromList moreLetters) command)

            else
                readShort moreLetters rest (withFlag name command)


withOption : String -> String -> Command -> Command
withOption name value command =
    case name of
        "request" ->
            { command | method = Just value }

        "header" ->
            case parseHeader value of
                Just header ->
                    { command | headers = command.headers ++ [ header ] }

                Nothing ->
                    command

        "url" ->
            setUrl value command

        "json" ->
            { command | data = command.data ++ [ value ], json = True }

        _ ->
            if List.member name dataOptions then
                { command | data = command.data ++ [ value ] }

            else
                command


withFlag : String -> Command -> Command
withFlag name command =
    case name of
        "get" ->
            { command | get = True }

        "head" ->
            { command | head = True }

        _ ->
            command


{-| curl fetches every URL it is given; a request has one, the first.
-}
setUrl : String -> Command -> Command
setUrl url command =
    if command.url == Nothing then
        { command | url = Just url }

    else
        command


parseHeader : String -> Maybe ( String, String )
parseHeader header =
    case String.indexes ":" header of
        index :: _ ->
            let
                key =
                    String.trim (String.left index header)
            in
            if String.isEmpty key then
                Nothing

            else
                Just ( key, String.trim (String.dropLeft (index + 1) header) )

        [] ->
            Nothing


{-| What curl would send. Data makes the request a POST unless a method is
given, or a GET with the data in the URL's query with `-G`. `--json` sends
its data as it is and says it is JSON, unless the headers already say what
it is.
-}
toRequest : Command -> Request
toRequest command =
    let
        data =
            String.join
                (if command.json then
                    ""

                 else
                    "&"
                )
                command.data

        url =
            Maybe.withDefault "" command.url

        defaultMethod =
            if command.head then
                "HEAD"

            else if command.get || List.isEmpty command.data then
                "GET"

            else
                "POST"

        headers =
            if command.json then
                command.headers
                    |> withDefaultHeader "Content-Type" "application/json"
                    |> withDefaultHeader "Accept" "application/json"

            else
                command.headers
    in
    if command.get then
        { method = Maybe.withDefault defaultMethod command.method
        , url = withQuery data url
        , headers = headers
        , body = ""
        }

    else
        { method = Maybe.withDefault defaultMethod command.method
        , url = url
        , headers = headers
        , body = data
        }


withDefaultHeader : String -> String -> List ( String, String ) -> List ( String, String )
withDefaultHeader key value headers =
    if List.any (\( existing, _ ) -> String.toLower existing == String.toLower key) headers then
        headers

    else
        headers ++ [ ( key, value ) ]


withQuery : String -> String -> String
withQuery query url =
    if String.isEmpty query then
        url

    else if String.contains "?" url then
        url ++ "&" ++ query

    else
        url ++ "?" ++ query



-- curl's options


dataOptions : List String
dataOptions =
    [ "data", "data-ascii", "data-binary", "data-raw", "data-urlencode" ]


longName : Char -> String
longName letter =
    case letter of
        'A' ->
            "user-agent"

        'b' ->
            "cookie"

        'c' ->
            "cookie-jar"

        'C' ->
            "continue-at"

        'd' ->
            "data"

        'D' ->
            "dump-header"

        'e' ->
            "referer"

        'E' ->
            "cert"

        'F' ->
            "form"

        'G' ->
            "get"

        'H' ->
            "header"

        'I' ->
            "head"

        'K' ->
            "config"

        'm' ->
            "max-time"

        'o' ->
            "output"

        'P' ->
            "ftp-port"

        'Q' ->
            "quote"

        'r' ->
            "range"

        't' ->
            "telnet-option"

        'T' ->
            "upload-file"

        'u' ->
            "user"

        'U' ->
            "proxy-user"

        'w' ->
            "write-out"

        'x' ->
            "proxy"

        'X' ->
            "request"

        'y' ->
            "speed-time"

        'Y' ->
            "speed-limit"

        'z' ->
            "time-cond"

        _ ->
            String.fromChar letter


{-| The options that take a value, by their long names. Any other option is
a flag: it never takes the word after it.
-}
takesValue : String -> Bool
takesValue name =
    List.member name dataOptions
        || List.member name
            [ "abstract-unix-socket"
            , "aws-sigv4"
            , "cacert"
            , "capath"
            , "cert"
            , "cert-type"
            , "ciphers"
            , "config"
            , "connect-timeout"
            , "connect-to"
            , "continue-at"
            , "cookie"
            , "cookie-jar"
            , "dns-servers"
            , "dump-header"
            , "expect100-timeout"
            , "form"
            , "form-string"
            , "ftp-port"
            , "header"
            , "interface"
            , "json"
            , "keepalive-time"
            , "key"
            , "key-type"
            , "limit-rate"
            , "local-port"
            , "max-filesize"
            , "max-redirs"
            , "max-time"
            , "oauth2-bearer"
            , "output"
            , "pass"
            , "proto"
            , "proto-redir"
            , "proxy"
            , "proxy-user"
            , "quote"
            , "range"
            , "referer"
            , "request"
            , "request-target"
            , "resolve"
            , "retry"
            , "retry-delay"
            , "retry-max-time"
            , "speed-limit"
            , "speed-time"
            , "stderr"
            , "telnet-option"
            , "time-cond"
            , "trace"
            , "trace-ascii"
            , "unix-socket"
            , "upload-file"
            , "url"
            , "url-query"
            , "user"
            , "user-agent"
            , "variable"
            , "write-out"
            ]



-- Splitting the command into words


tokenize : String -> List String
tokenize command =
    tokenizeChars (String.toList command) Nothing []


{-| The word being read is kept reversed, and is `Nothing` between words, so
that an empty quoted word (`''`) is still a word.
-}
tokenizeChars : List Char -> Maybe (List Char) -> List String -> List String
tokenizeChars chars current words =
    case chars of
        [] ->
            List.reverse (pushWord current words)

        '\\' :: '\u{000D}' :: '\n' :: rest ->
            tokenizeChars rest current words

        '\\' :: '\n' :: rest ->
            tokenizeChars rest current words

        '\\' :: char :: rest ->
            tokenizeChars rest (Just (char :: Maybe.withDefault [] current)) words

        '\'' :: rest ->
            let
                ( word, afterQuote ) =
                    singleQuoted rest (Maybe.withDefault [] current)
            in
            tokenizeChars afterQuote (Just word) words

        '$' :: '\'' :: rest ->
            let
                ( word, afterQuote ) =
                    ansiQuoted rest (Maybe.withDefault [] current)
            in
            tokenizeChars afterQuote (Just word) words

        '"' :: rest ->
            let
                ( word, afterQuote ) =
                    doubleQuoted rest (Maybe.withDefault [] current)
            in
            tokenizeChars afterQuote (Just word) words

        char :: rest ->
            if isSpace char then
                tokenizeChars rest Nothing (pushWord current words)

            else
                tokenizeChars rest (Just (char :: Maybe.withDefault [] current)) words


pushWord : Maybe (List Char) -> List String -> List String
pushWord current words =
    case current of
        Just chars ->
            String.fromList (List.reverse chars) :: words

        Nothing ->
            words


isSpace : Char -> Bool
isSpace char =
    char == ' ' || char == '\t' || char == '\n' || char == '\u{000D}'


singleQuoted : List Char -> List Char -> ( List Char, List Char )
singleQuoted chars word =
    case chars of
        [] ->
            ( word, [] )

        '\'' :: rest ->
            ( word, rest )

        char :: rest ->
            singleQuoted rest (char :: word)


doubleQuoted : List Char -> List Char -> ( List Char, List Char )
doubleQuoted chars word =
    case chars of
        [] ->
            ( word, [] )

        '"' :: rest ->
            ( word, rest )

        '\\' :: '\n' :: rest ->
            doubleQuoted rest word

        '\\' :: char :: rest ->
            if List.member char [ '\\', '"', '$', '`' ] then
                doubleQuoted rest (char :: word)

            else
                doubleQuoted rest (char :: '\\' :: word)

        char :: rest ->
            doubleQuoted rest (char :: word)


ansiQuoted : List Char -> List Char -> ( List Char, List Char )
ansiQuoted chars word =
    case chars of
        [] ->
            ( word, [] )

        '\'' :: rest ->
            ( word, rest )

        '\\' :: 'x' :: rest ->
            ansiCode 2 rest word

        '\\' :: 'u' :: rest ->
            ansiCode 4 rest word

        '\\' :: 'U' :: rest ->
            ansiCode 8 rest word

        '\\' :: char :: rest ->
            ansiQuoted rest (ansiEscape char :: word)

        char :: rest ->
            ansiQuoted rest (char :: word)


ansiEscape : Char -> Char
ansiEscape char =
    case char of
        'n' ->
            '\n'

        't' ->
            '\t'

        'r' ->
            '\u{000D}'

        'a' ->
            '\u{0007}'

        'b' ->
            '\u{0008}'

        'e' ->
            '\u{001B}'

        'E' ->
            '\u{001B}'

        'f' ->
            '\u{000C}'

        'v' ->
            '\u{000B}'

        _ ->
            char


{-| `\xHH`, `\uHHHH` and `\UHHHHHHHH`: up to that many hexadecimal digits.
-}
ansiCode : Int -> List Char -> List Char -> ( List Char, List Char )
ansiCode maxDigits chars word =
    let
        digits =
            takeHexDigits maxDigits chars []

        rest =
            List.drop (List.length digits) chars
    in
    if List.isEmpty digits then
        ansiQuoted rest word

    else
        ansiQuoted rest (Char.fromCode (hexValue digits) :: word)


takeHexDigits : Int -> List Char -> List Char -> List Char
takeHexDigits remaining chars digits =
    case chars of
        char :: rest ->
            if remaining > 0 && Char.isHexDigit char then
                takeHexDigits (remaining - 1) rest (char :: digits)

            else
                List.reverse digits

        [] ->
            List.reverse digits


hexValue : List Char -> Int
hexValue =
    List.foldl (\char value -> value * 16 + hexDigitValue char) 0


hexDigitValue : Char -> Int
hexDigitValue char =
    if Char.isDigit char then
        Char.toCode char - Char.toCode '0'

    else
        Char.toCode (Char.toLower char) - Char.toCode 'a' + 10
