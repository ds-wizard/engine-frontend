module UserAgent exposing (Browser, browserToString, getBrowser, getOS, isMobile, osToString)

import Regex exposing (Regex)


type Browser
    = Chrome
    | Firefox
    | Safari
    | Opera
    | Edge
    | InternetExplorer
    | UnknownBrowser


type OS
    = Windows
    | MacOS
    | IOS
    | Android
    | Linux
    | UnknownOS


browserToString : Browser -> String
browserToString browser =
    case browser of
        Chrome ->
            "Chrome"

        Firefox ->
            "Firefox"

        Safari ->
            "Safari"

        Opera ->
            "Opera"

        Edge ->
            "Edge"

        InternetExplorer ->
            "Internet Explorer"

        UnknownBrowser ->
            "Unknown Browser"


osToString : OS -> String
osToString os =
    case os of
        Windows ->
            "Windows"

        MacOS ->
            "macOS"

        IOS ->
            "iOS"

        Android ->
            "Android"

        Linux ->
            "Linux"

        UnknownOS ->
            "Unknown Operating System"


isMobile : OS -> Bool
isMobile os =
    case os of
        IOS ->
            True

        Android ->
            True

        _ ->
            False


getBrowser : String -> Browser
getBrowser userAgent =
    if Regex.contains edgeRegex userAgent then
        Edge

    else if Regex.contains chromeRegex userAgent then
        Chrome

    else if Regex.contains firefoxRegex userAgent then
        Firefox

    else if Regex.contains safariRegex userAgent then
        Safari

    else if Regex.contains operaRegex userAgent then
        Opera

    else if Regex.contains internetExplorerRegex userAgent then
        InternetExplorer

    else
        UnknownBrowser


getOS : String -> OS
getOS userAgent =
    if Regex.contains windowsRegex userAgent then
        Windows

    else if Regex.contains iosRegex userAgent then
        IOS

    else if Regex.contains macosRegex userAgent then
        MacOS

    else if Regex.contains androidRegex userAgent then
        Android

    else if Regex.contains linuxRegex userAgent then
        Linux

    else
        UnknownOS



-- Browser Regexes


chromeRegex : Regex
chromeRegex =
    createRegex "chrome|chromium|crios"


firefoxRegex : Regex
firefoxRegex =
    createRegex "firefox|fxios"


safariRegex : Regex
safariRegex =
    createRegex "safari"


operaRegex : Regex
operaRegex =
    createRegex "opr/"


edgeRegex : Regex
edgeRegex =
    createRegex "edg/"


internetExplorerRegex : Regex
internetExplorerRegex =
    createRegex "msie|rw:"



-- OS Regexes


windowsRegex : Regex
windowsRegex =
    createRegex "win"


macosRegex : Regex
macosRegex =
    createRegex "mac"


iosRegex : Regex
iosRegex =
    createRegex "iphone|ipad|ipod"


androidRegex : Regex
androidRegex =
    createRegex "android"


linuxRegex : Regex
linuxRegex =
    createRegex "linux"


createRegex : String -> Regex
createRegex =
    Maybe.withDefault Regex.never
        << Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
