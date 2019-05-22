module KnowledgeModels.Common.Version exposing
    ( Version(..)
    , compare
    , create
    , decoder
    , toString
    )

import Json.Decode as D exposing (Decoder)


type Version
    = Version Int Int Int


create : Int -> Int -> Int -> Version
create =
    Version


decoder : Decoder Version
decoder =
    D.string
        |> D.andThen
            (\str ->
                let
                    parts =
                        String.split "." str
                            |> List.map String.toInt
                in
                case parts of
                    (Just major) :: (Just minor) :: (Just patch) :: [] ->
                        D.succeed <| Version major minor patch

                    _ ->
                        D.fail <| "Invalid version " ++ str
            )


toString : Version -> String
toString (Version major minor patch) =
    String.fromInt major ++ "." ++ String.fromInt minor ++ "." ++ String.fromInt patch


compare : Version -> Version -> Order
compare (Version major1 minor1 patch1) (Version major2 minor2 patch2) =
    if major1 < major2 then
        LT

    else if major1 > major2 then
        GT

    else if minor1 < minor2 then
        LT

    else if minor1 > minor2 then
        GT

    else if patch1 < patch2 then
        LT

    else if patch1 > patch2 then
        GT

    else
        EQ
