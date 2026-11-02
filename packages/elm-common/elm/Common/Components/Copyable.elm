module Common.Components.Copyable exposing
    ( brief
    , button
    , codeBlock
    , inline
    , link
    )

{-| A value with a button that puts it on the clipboard, in the language of the
page.

`Ui.Copy` does the work — the tooltip that says "Click to copy" and then
"Copied!", on hover and on focus alike, and the live region that announces the
copy. This supplies the words, from the msgids the existing copy buttons use.

`label` names what is copied — "Copy the API key" — and becomes the button's
accessible name. A page with several of these otherwise has several buttons
all called "Copy", and a screen reader cannot tell them apart by position.

`key` tells two buttons on a page apart, so that pressing one does not make
every other one say "Copied!". Give each a different one.

-}

import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Ui.CodeBlock as CodeBlock
import Ui.Copy as Copy


{-| The button on its own, where what it copies is not shown: the identifier of
whatever a knowledge model editor has open, which is copied to be pasted into a
document template and never read off the screen. `label` has to say what the
value is, since nothing on the screen does.
-}
button :
    WithLocale a
    -> { key : String, label : String, value : String, model : Copy.Model, toMsg : Copy.Msg -> msg }
    -> Html msg
button appState config =
    Copy.buttonWithWords (words appState) config


{-| The button with the first `chars` characters of what it copies inside it —
the identifier of whatever a knowledge model editor has open, enough to see
which one is on offer without a uuid across the heading.

`label` names what is copied and is what the tooltip says, so it reads as an
instruction: "Copy UUID".

-}
brief :
    WithLocale a
    -> { key : String, label : String, chars : Int, value : String, model : Copy.Model, toMsg : Copy.Msg -> msg }
    -> Html msg
brief appState config =
    Copy.briefWithWords
        { prompt = config.label, done = gettext "Copied!" appState.locale }
        config.chars
        { key = config.key
        , label = config.label
        , value = config.value
        , model = config.model
        , toMsg = config.toMsg
        }


{-| An identifier or a token beside its copy button.
-}
inline :
    WithLocale a
    -> { key : String, label : String, value : String, model : Copy.Model, toMsg : Copy.Msg -> msg }
    -> Html msg
inline appState config =
    Copy.inlineWithWords (words appState) config


{-| "Copy link", which says "Copied!" once pressed — for a link that is there to
be passed on rather than read, the public link of a project. `label` is the
button's words before the press.
-}
link :
    WithLocale a
    -> { key : String, label : String, value : String, model : Copy.Model, toMsg : Copy.Msg -> msg }
    -> Html msg
link appState config =
    Copy.textButtonWithWords (words appState) config


{-| A block of code with the copy button in its corner. `label` names the code
for the editor and the button alike — "Handler code" becomes the code region's
name, and the button's is "Copy Handler code".
-}
codeBlock :
    WithLocale a
    -> { key : String, label : String, language : String, value : String, model : Copy.Model, toMsg : Copy.Msg -> msg }
    -> Html msg
codeBlock appState config =
    CodeBlock.withActions
        [ Copy.buttonWithWords (words appState)
            { key = config.key
            , label = String.format (gettext "Copy %s" appState.locale) [ config.label ]
            , value = config.value
            , model = config.model
            , toMsg = config.toMsg
            }
        ]
        { label = config.label
        , language = config.language
        , value = config.value
        }


words : WithLocale a -> Copy.Words
words appState =
    { prompt = gettext "Click to copy" appState.locale
    , done = gettext "Copied!" appState.locale
    }
