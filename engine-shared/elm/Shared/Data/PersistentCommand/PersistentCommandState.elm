module Shared.Data.PersistentCommand.PersistentCommandState exposing
    ( PersistentCommandState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type PersistentCommandState
    = New
    | Done
    | Error
    | Ignore


fromString : String -> Maybe PersistentCommandState
fromString str =
    case str of
        "NewPersistentCommandState" ->
            Just New

        "DonePersistentCommandState" ->
            Just Done

        "ErrorPersistentCommandState" ->
            Just Error

        "IgnorePersistentCommandState" ->
            Just Ignore

        _ ->
            Nothing


decoder : Decoder PersistentCommandState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just visibility ->
                        D.succeed visibility

                    Nothing ->
                        D.fail <| "Unknown persistent command state: " ++ str
            )
