module Common.View.ItemIcon exposing
    ( Config
    , view
    )

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src, style)


type alias Config =
    { text : String
    , image : Maybe String
    }


view : Config -> Html msg
view config =
    let
        letter =
            config.text
                |> String.uncons
                |> Maybe.map (\( a, _ ) -> a)
                |> Maybe.withDefault 'A'

        hash =
            config.text
                |> String.toList
                |> List.map Char.toCode
                |> List.sum

        h =
            String.fromInt <| remainderBy 360 hash

        s =
            String.fromInt <| 25 + remainderBy 71 hash

        l =
            String.fromInt <| 85 + remainderBy 11 hash

        hsl =
            "hsl(" ++ h ++ "," ++ s ++ "%," ++ l ++ "%)"

        ( backgroundColorStyle, content ) =
            case config.image of
                Just image ->
                    ( []
                    , img [ src image ] []
                    )

                Nothing ->
                    ( [ style "background-color" hsl ]
                    , text <| String.fromChar letter
                    )
    in
    div ([ class "ItemIcon" ] ++ backgroundColorStyle)
        [ content ]
