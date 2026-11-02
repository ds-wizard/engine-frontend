module Common.Components.DiffText exposing (lines, words)

{-| What changed inside a value, marked up where it changed.

`<ins>` and `<del>` are announced as an insertion and a deletion, so nothing has
to be written to say so, and they carry the success and danger subtle roles for
the eye.

Two places show this: the comparison of two knowledge models, and the migration
of an editor onto a newer parent. They were written twice and are written once
here, so a change to how a difference reads happens in one place.

-}

import Diff
import Html exposing (Html, del, ins, pre, span, text)
import Html.Attributes exposing (class)
import List.Extra as List


{-| A value that is the same on both sides, drawn as itself. It takes the same
wrapper as the others so a row of values lines up whether or not it changed.
-}
unchanged : String -> Html msg
unchanged value =
    span [ class "c-diff" ] [ text value ]


{-| Two pieces of prose, diffed a word at a time. Runs of the same kind are
joined first, so a changed sentence is one insertion rather than one per word.

Unchanged values are drawn plainly rather than as an insertion of everything:
a field nobody touched should read as a field nobody touched.

-}
words : String -> String -> Html msg
words before after =
    if before == after then
        unchanged after

    else
        let
            split value =
                let
                    parts =
                        String.split " " value

                    last =
                        List.length parts - 1
                in
                List.indexedMap
                    (\index word ->
                        if index == last then
                            word

                        else
                            word ++ " "
                    )
                    parts

            merge change changes =
                case ( List.last changes, change ) of
                    ( Just (Diff.Added earlier), Diff.Added now ) ->
                        dropLast changes ++ [ Diff.Added (earlier ++ now) ]

                    ( Just (Diff.Removed earlier), Diff.Removed now ) ->
                        dropLast changes ++ [ Diff.Removed (earlier ++ now) ]

                    ( Just (Diff.NoChange earlier), Diff.NoChange now ) ->
                        dropLast changes ++ [ Diff.NoChange (earlier ++ now) ]

                    _ ->
                        changes ++ [ change ]

            dropLast =
                List.reverse >> List.drop 1 >> List.reverse

            draw change =
                case change of
                    Diff.Added value ->
                        ins [] [ text value ]

                    Diff.Removed value ->
                        del [] [ text value ]

                    Diff.NoChange value ->
                        text value
        in
        span [ class "c-diff" ]
            (List.map draw (List.foldl merge [] (Diff.diff (split before) (split after))))


{-| Two blocks of code or text, diffed a line at a time — an automation's
code before and after a change. Each line that came or went is a whole
insertion or deletion, so a screen reader says which lines changed and the eye
sees them as bands; the lines in between are drawn as they are.
-}
lines : String -> String -> Html msg
lines before after =
    let
        -- An empty line keeps its height, so the bands stay where the lines are.
        line value =
            if String.isEmpty value then
                text "\u{00A0}"

            else
                text value

        draw change =
            case change of
                Diff.Added value ->
                    ins [] [ line value ]

                Diff.Removed value ->
                    del [] [ line value ]

                Diff.NoChange value ->
                    span [] [ line value ]
    in
    pre [ class "c-diff c-diff--lines" ] (List.map draw (Diff.diffLines before after))
