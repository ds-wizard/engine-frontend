module Shared.Data.PersistentCommandState exposing
    ( PersistentCommandState(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


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


toString : PersistentCommandState -> String
toString state =
    case state of
        New ->
            "NewPersistentCommandState"

        Done ->
            "DonePersistentCommandState"

        Error ->
            "ErrorPersistentCommandState"

        Ignore ->
            "IgnorePersistentCommandState"


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


encode : PersistentCommandState -> E.Value
encode =
    E.string << toString
