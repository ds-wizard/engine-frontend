module Shared.Data.Submission.SubmissionState exposing
    ( SubmissionState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type SubmissionState
    = InProgress
    | Done
    | Error


fromString : String -> Maybe SubmissionState
fromString str =
    case str of
        "InProgressSubmissionState" ->
            Just InProgress

        "DoneSubmissionState" ->
            Just Done

        "ErrorSubmissionState" ->
            Just Error

        _ ->
            Nothing


decoder : Decoder SubmissionState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just visibility ->
                        D.succeed visibility

                    Nothing ->
                        D.fail <| "Unknown submission state: " ++ str
            )
