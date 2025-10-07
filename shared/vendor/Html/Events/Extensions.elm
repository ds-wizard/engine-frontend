module Html.Events.Extensions exposing
    ( alwaysPreventDefaultOn
    , alwaysPreventDefaultOnWithDecoder
    , onBlurWithSelection
    , onKeyConfirm
    )

import Html
import Html.Events exposing (on, preventDefaultOn)
import Json.Decode as D exposing (Decoder)


alwaysPreventDefaultOn : String -> msg -> Html.Attribute msg
alwaysPreventDefaultOn event msg =
    preventDefaultOn event (D.succeed ( msg, True ))


alwaysPreventDefaultOnWithDecoder : String -> D.Decoder msg -> Html.Attribute msg
alwaysPreventDefaultOnWithDecoder event decoder =
    preventDefaultOn event (D.map (\msg -> ( msg, True )) decoder)


onBlurWithSelection : (Int -> Int -> msg) -> Html.Attribute msg
onBlurWithSelection tagger =
    let
        decoder =
            D.map2 tagger
                (D.at [ "target", "selectionStart" ] D.int)
                (D.at [ "target", "selectionEnd" ] D.int)
    in
    on "blur" decoder


onKeyConfirm : msg -> Html.Attribute msg
onKeyConfirm msg =
    on "keydown" (confirmKeyDecoder msg)


confirmKeyDecoder : msg -> Decoder msg
confirmKeyDecoder msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" || key == " " then
                    D.succeed msg

                else
                    D.fail "Not enter or space"
            )
