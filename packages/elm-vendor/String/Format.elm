module String.Format exposing
    ( format
    , formatHtml
    )

import Html exposing (Html, text)


format : String -> List String -> String
format str =
    format_ (String.toList str) >> String.fromList


format_ : List Char -> List String -> List Char
format_ chars terms =
    case chars of
        '%' :: 's' :: rest ->
            (String.toList <| Maybe.withDefault "%s" <| List.head terms)
                ++ (format_ rest <| List.drop 1 terms)

        '%' :: '%' :: 's' :: rest ->
            '%' :: 's' :: format_ rest terms

        a :: rest ->
            a :: format_ rest terms

        [] ->
            []


formatHtml : String -> List (Html msg) -> List (Html msg)
formatHtml str =
    formatHtml_ "" (String.toList str)


formatHtml_ : String -> List Char -> List (Html msg) -> List (Html msg)
formatHtml_ currentText chars elements =
    let
        toHtml str =
            if String.isEmpty str then
                []

            else
                [ text currentText ]
    in
    case chars of
        '%' :: 's' :: rest ->
            case List.head elements of
                Just element ->
                    toHtml currentText ++ [ element ] ++ formatHtml_ "" rest (List.drop 1 elements)

                Nothing ->
                    formatHtml_ (currentText ++ "%s") rest []

        '%' :: '%' :: 's' :: rest ->
            formatHtml_ (currentText ++ "%s") rest elements

        a :: rest ->
            formatHtml_ (currentText ++ String.fromChar a) rest elements

        [] ->
            toHtml currentText
