module Wizard.Common.View.ItemIcon exposing
    ( Config
    , ConfigExtra
    , IconFaConfig
    , iconFa
    , view
    , viewExtra
    )

import Html exposing (Attribute, Html, div, img, text)
import Html.Attributes exposing (class, src, style)


type alias Config =
    { text : String
    , image : Maybe String
    }


view : Config -> Html msg
view config =
    viewExtra
        { text = config.text
        , image = config.image
        , attributes = []
        }


type alias ConfigExtra msg =
    { text : String
    , image : Maybe String
    , attributes : List (Attribute msg)
    }


viewExtra : ConfigExtra msg -> Html msg
viewExtra config =
    let
        ( backgroundColorStyle, content ) =
            case config.image of
                Just image ->
                    ( []
                    , img [ src image ] []
                    )

                Nothing ->
                    let
                        hash =
                            config.text
                                |> String.toList
                                |> List.map Char.toCode
                                |> List.sum

                        l =
                            String.fromInt <| 85 + remainderBy 11 hash

                        s =
                            String.fromInt <| 25 + remainderBy 71 hash

                        h =
                            String.fromInt <| remainderBy 360 hash

                        hsl =
                            "hsl(" ++ h ++ "," ++ s ++ "%," ++ l ++ "%)"

                        letter =
                            config.text
                                |> String.uncons
                                |> Maybe.map (\( a, _ ) -> a)
                                |> Maybe.withDefault 'A'
                    in
                    ( [ style "background-color" hsl ]
                    , text <| String.fromChar letter
                    )
    in
    div (class "ItemIcon" :: backgroundColorStyle ++ config.attributes)
        [ content ]



-- Font Awesome Icon


type alias IconFaConfig msg =
    { icon : Html msg
    , extraClass : Maybe String
    }


iconFa : IconFaConfig msg -> Html msg
iconFa cfg =
    let
        extraClass =
            Maybe.withDefault "" cfg.extraClass
    in
    div [ class ("ItemIcon " ++ extraClass) ]
        [ cfg.icon ]
