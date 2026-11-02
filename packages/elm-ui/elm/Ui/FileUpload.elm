module Ui.FileUpload exposing
    ( Model
    , Msg
    , Words
    , clear
    , file
    , fileContent
    , files
    , init
    , toHtml
    , toHtmlWithError
    , toHtmlWithErrorAndTestId
    , toHtmlWithWords
    , update
    , updateMultiple
    )

{-| Choosing a file, by button or by dropping one.

`Dropzone` and `FileImport` exist twice each today and do the same thing. The
behavior here is theirs; what is different is that dropping is never the only
way in and never the only thing announced:

  - **The button is the control.** A drop target is a pointer gesture, and a
    gesture cannot be the only route to a function (2.1.1, and 2.5.7 for the
    drag). The button opens the same picker and is where focus lands.
  - **The chosen file is announced.** The existing dropzone swaps its contents
    for the file name, which a screen reader has no reason to look at. Here the
    name sits in a live region, so choosing a file says so.
  - **The drop target is not a button.** It has no role and no tab stop, because
    it is an extra way to do what the button does — inventing a role for it would
    add a stop that leads somewhere the keyboard cannot use.

-}

import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (attribute, class, classList, for, id, type_)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Task
import Ui.Icon as Icon
import Ui.TestId as TestId



-- MODEL


type Model
    = Model
        { hover : Bool
        , file : Maybe File
        , files : List File
        , content : Maybe String
        }


init : Model
init =
    Model { hover = False, file = Nothing, files = [], content = Nothing }


file : Model -> Maybe File
file (Model model) =
    model.file


{-| Every chosen file, in the order they were given. With `update` that is the
one file or nothing; with `updateMultiple` it is however many were picked or
dropped.
-}
files : Model -> List File
files (Model model) =
    model.files


{-| The file's text, for the callers that read it rather than upload it — a
knowledge model being imported, say. Only present when `update` was told to read.
-}
fileContent : Model -> Maybe String
fileContent (Model model) =
    model.content


{-| Back to nothing chosen. A function rather than `init` at the call site, so a
caller asking for "no file" does not have to know that is the same as a new one.
-}
clear : Model
clear =
    init



-- UPDATE


type Msg
    = Picked
    | DragEntered
    | DragLeft
    | GotFile File
    | GotFiles File (List File)
    | GotContent String


update : { mimes : List String, read : Bool } -> Msg -> Model -> ( Model, Cmd Msg )
update config msg (Model model) =
    case msg of
        Picked ->
            ( Model model, Select.file config.mimes GotFile )

        DragEntered ->
            ( Model { model | hover = True }, Cmd.none )

        DragLeft ->
            ( Model { model | hover = False }, Cmd.none )

        GotFile chosen ->
            ( Model { model | hover = False, file = Just chosen, files = [ chosen ] }
            , if config.read then
                Task.perform GotContent (File.toString chosen)

              else
                Cmd.none
            )

        GotFiles chosen _ ->
            -- Several dropped on a field that takes one: the first, as the
            -- picker would have allowed.
            update config (GotFile chosen) (Model model)

        GotContent content ->
            ( Model { model | content = Just content }, Cmd.none )


{-| The same, for a control that takes several files at once — importing a batch
of packages, say. The picker allows a multiple selection and a drop keeps every
file. Nothing is read: several files are uploaded, not parsed in the browser.
-}
updateMultiple : { mimes : List String } -> Msg -> Model -> ( Model, Cmd Msg )
updateMultiple config msg (Model model) =
    case msg of
        Picked ->
            ( Model model, Select.files config.mimes GotFiles )

        GotFile chosen ->
            ( Model { model | hover = False, file = Just chosen, files = [ chosen ] }, Cmd.none )

        GotFiles chosen rest ->
            ( Model { model | hover = False, file = Just chosen, files = chosen :: rest }, Cmd.none )

        _ ->
            update { mimes = config.mimes, read = False } msg (Model model)



-- VIEW


{-| The label is shown above the area, whichever state it is in, and names the
button: the button's accessible name is the label followed by its own words —
"Knowledge model, Choose a file" — so a screen reader reaching it knows what the
file is for, not only that one can be chosen. The ids that association needs are
made from the label; where two uploads on a page share a label, or the error
slot is wanted, use `toHtmlWithError`, which takes an id.
-}
toHtml :
    { toMsg : Msg -> msg
    , label : String
    , buttonLabel : String
    , hint : String
    , invalid : Bool
    , onClear : Maybe msg
    }
    -> Model
    -> Html msg
toHtml =
    toHtmlWithWords english


{-| What the control says of its own accord once something is chosen.

  - `chosen` names what was chosen, given the first file's name and how many
    others came with it — "data.json", or "data.json (+2)".
  - `remove` names the button that clears the choice, given the first file's name.

`toHtml` says both in English.

-}
type alias Words =
    { chosen : String -> Int -> String
    , remove : String -> String
    }


english : Words
english =
    { chosen =
        \name others ->
            if others > 0 then
                name ++ " (+" ++ String.fromInt others ++ ")"

            else
                name
    , remove = \name -> "Remove " ++ name
    }


{-| `toHtml`, with the words given.
-}
toHtmlWithWords :
    Words
    ->
        { toMsg : Msg -> msg
        , label : String
        , buttonLabel : String
        , hint : String
        , invalid : Bool
        , onClear : Maybe msg
        }
    -> Model
    -> Html msg
toHtmlWithWords words config =
    view words
        { id = "ui-file-upload-" ++ slug config.label
        , toMsg = config.toMsg
        , label = config.label
        , buttonLabel = config.buttonLabel
        , hint = config.hint
        , invalid = config.invalid
        , error = Nothing
        , onClear = config.onClear
        , testId = Nothing
        }


{-| The upload as a form field: an `id`, and an error said in words under it —
"File is required."

The error is what `invalid` only hinted at. It is in the button's
`aria-describedby`, next to the hint, so reaching the button says what is wrong;
the area takes the invalid border as well, and the message carries the same
icon a `Ui.Field` error does, so the color is never the only signal (1.4.1).
When a file has been chosen the error is still shown, under the chosen file, and
describes the remove button.

The ids of the label, the hint and the error are made from `id`, which must be
unique on the page. The button itself is `<id>-button`, the target for moving
focus to the first thing wrong.

-}
toHtmlWithError :
    Words
    ->
        { id : String
        , toMsg : Msg -> msg
        , label : String
        , buttonLabel : String
        , hint : String
        , error : Maybe String
        , onClear : Maybe msg
        }
    -> Model
    -> Html msg
toHtmlWithError words config =
    toHtmlWithErrorAndTestId words
        { id = config.id
        , toMsg = config.toMsg
        , label = config.label
        , buttonLabel = config.buttonLabel
        , hint = config.hint
        , error = config.error
        , onClear = config.onClear
        , testId = Nothing
        }


{-| `toHtmlWithError`, with a `data-cy` on the field for the end-to-end tests.

The area a file is dropped on is `file-upload_dropzone` whichever way the upload
is drawn: the drop events are on it, not on the field, so that is where a test
drops. The field's test id scopes it where a page has more than one.

-}
toHtmlWithErrorAndTestId :
    Words
    ->
        { id : String
        , toMsg : Msg -> msg
        , label : String
        , buttonLabel : String
        , hint : String
        , error : Maybe String
        , onClear : Maybe msg
        , testId : Maybe String
        }
    -> Model
    -> Html msg
toHtmlWithErrorAndTestId words config =
    view words
        { id = config.id
        , toMsg = config.toMsg
        , label = config.label
        , buttonLabel = config.buttonLabel
        , hint = config.hint
        , invalid = Maybe.isJust config.error
        , error = config.error
        , onClear = config.onClear
        , testId = config.testId
        }


type alias ViewConfig msg =
    { id : String
    , toMsg : Msg -> msg
    , label : String
    , buttonLabel : String
    , hint : String
    , invalid : Bool
    , error : Maybe String
    , onClear : Maybe msg
    , testId : Maybe String
    }


view : Words -> ViewConfig msg -> Model -> Html msg
view words config (Model model) =
    div
        (class "ui-field"
            :: TestId.attributes config.testId
        )
        [ -- A label for the button: `for` makes pressing it open the picker, as a
          -- label does for a native file input, and `aria-labelledby` on the
          -- button keeps the button's own words in its name, which a label alone
          -- would replace.
          Html.label [ class "ui-field__label", id (labelId config), for (buttonId config) ] [ text config.label ]
        , case model.file of
            Just chosen ->
                viewChosen words config chosen (List.length model.files)

            Nothing ->
                viewEmpty config model
        , case config.error of
            Just error ->
                p [ class "ui-field__error", id (errorId config) ]
                    [ span [ class "ui-field__error-icon", attribute "aria-hidden" "true" ] [ text "!" ]
                    , text error
                    ]

            Nothing ->
                text ""
        ]


viewEmpty : ViewConfig msg -> { b | hover : Bool } -> Html msg
viewEmpty config model =
    div
        [ classList
            [ ( "ui-file-upload", True )
            , ( "is-hovered", model.hover )
            , ( "is-invalid", config.invalid )
            ]

        -- No role and no tabindex. Dropping is a second way to do what the
        -- button does, so it is not a control of its own.
        , TestId.attribute "file-upload_dropzone"
        , always DragEntered |> preventDefaultOnMsg "dragenter" config.toMsg
        , always DragEntered |> preventDefaultOnMsg "dragover" config.toMsg
        , always DragLeft |> preventDefaultOnMsg "dragleave" config.toMsg
        , preventDefaultOn "drop" (Decode.map (\( chosen, rest ) -> ( config.toMsg (GotFiles chosen rest), True )) droppedFiles)
        ]
        [ span [ class "ui-file-upload__icon", attribute "aria-hidden" "true" ]
            [ Icon.toHtml (Icon.solid "fa-file-arrow-up") ]

        -- Drawn here rather than with `Ui.Button`, which has no id and no
        -- labeling of its own: the label and the description have to point
        -- at this element.
        , button
            [ type_ "button"
            , id (buttonId config)
            , class "ui-button ui-button--secondary"
            , attribute "aria-labelledby" (labelId config ++ " " ++ buttonId config)

            -- Not `aria-invalid`, which a button does not take: the error is
            -- in the description, which is where a reader hears it.
            , attribute "aria-describedby" (describedBy config True)
            , onClick (config.toMsg Picked)
            ]
            [ span [] [ text config.buttonLabel ] ]
        , p [ class "ui-file-upload__hint", id (hintId config) ] [ text config.hint ]
        ]


{-| What was chosen, said out loud. A region that replaces itself silently is a
region a screen reader user has no reason to revisit.
-}
viewChosen : Words -> ViewConfig msg -> File -> Int -> Html msg
viewChosen words config chosen count =
    div [ class "ui-file-upload ui-file-upload--chosen" ]
        [ span [ class "ui-file-upload__icon", attribute "aria-hidden" "true" ]
            [ Icon.toHtml (Icon.solid "fa-file") ]
        , p
            [ class "ui-file-upload__name"
            , attribute "role" "status"
            ]
            [ text (words.chosen (File.name chosen) (count - 1)) ]
        , case config.onClear of
            Just onClear ->
                button
                    ([ type_ "button"
                     , class "ui-button ui-button--secondary ui-button--icon"
                     , attribute "aria-label" (words.remove (File.name chosen))
                     , onClick onClear
                     ]
                        ++ (case describedBy config False of
                                "" ->
                                    []

                                ids ->
                                    [ attribute "aria-describedby" ids ]
                           )
                    )
                    [ Icon.toHtml (Icon.solid "fa-xmark") ]

            Nothing ->
                text ""
        ]


{-| The hint only while it is on screen, then the error if there is one.
-}
describedBy : ViewConfig msg -> Bool -> String
describedBy config withHint =
    [ if withHint then
        Just (hintId config)

      else
        Nothing
    , Maybe.map (always (errorId config)) config.error
    ]
        |> Maybe.values
        |> String.join " "


labelId : ViewConfig msg -> String
labelId config =
    config.id ++ "-label"


buttonId : ViewConfig msg -> String
buttonId config =
    config.id ++ "-button"


hintId : ViewConfig msg -> String
hintId config =
    config.id ++ "-hint"


errorId : ViewConfig msg -> String
errorId config =
    config.id ++ "-error"


{-| Letters and digits of a label, lower-cased and joined by hyphens: "PO file"
is `po-file`. Only for the ids `toHtml` has to invent.
-}
slug : String -> String
slug label =
    label
        |> String.toLower
        |> String.map
            (\char ->
                if Char.isAlphaNum char then
                    char

                else
                    ' '
            )
        |> String.words
        |> String.join "-"


preventDefaultOnMsg : String -> (Msg -> msg) -> (() -> Msg) -> Html.Attribute msg
preventDefaultOnMsg event toMsg msg =
    preventDefaultOn event (Decode.succeed ( toMsg (msg ()), True ))


{-| Every dropped file. `dataTransfer.files` is a `FileList`, which is not an
array, so it is read by index up to its `length` rather than with `Decode.list`.
-}
droppedFiles : Decode.Decoder ( File, List File )
droppedFiles =
    Decode.at [ "dataTransfer", "files", "length" ] Decode.int
        |> Decode.andThen
            (\count ->
                if count < 1 then
                    Decode.fail "nothing was dropped"

                else
                    Decode.map2 Tuple.pair
                        (Decode.at [ "dataTransfer", "files", "0" ] File.decoder)
                        (List.range 1 (count - 1)
                            |> List.map (\index -> Decode.at [ "dataTransfer", "files", String.fromInt index ] File.decoder)
                            |> List.foldr (Decode.map2 (::)) (Decode.succeed [])
                        )
            )
