module Shared.Data.TenantState exposing
    ( TenantState(..)
    , decoder
    , filterOptions
    , toReadableString
    )

import Json.Decode as D exposing (Decoder)
import Maybe.Extra as Maybe


type TenantState
    = NotSeeded
    | PendingHousekeeping
    | HousekeepingInProgress
    | ReadyForUse


all : List TenantState
all =
    [ NotSeeded, PendingHousekeeping, HousekeepingInProgress, ReadyForUse ]


decoder : Decoder TenantState
decoder =
    D.string
        |> D.andThen
            (\state ->
                fromString state
                    |> Maybe.unwrap (D.fail ("Invalid tenant state " ++ state)) D.succeed
            )


toString : TenantState -> String
toString state =
    case state of
        NotSeeded ->
            "NotSeededTenantState"

        PendingHousekeeping ->
            "PendingHousekeepingTenantState"

        HousekeepingInProgress ->
            "HousekeepingInProgressTenantState"

        ReadyForUse ->
            "ReadyForUseTenantState"


fromString : String -> Maybe TenantState
fromString state =
    case state of
        "NotSeededTenantState" ->
            Just NotSeeded

        "PendingHousekeepingTenantState" ->
            Just PendingHousekeeping

        "HousekeepingInProgressTenantState" ->
            Just HousekeepingInProgress

        "ReadyForUseTenantState" ->
            Just ReadyForUse

        _ ->
            Nothing


filterOptions : List ( String, String )
filterOptions =
    let
        toOption state =
            ( toString state, toReadableString state )
    in
    List.map toOption all


toReadableString : TenantState -> String
toReadableString state =
    case state of
        NotSeeded ->
            "Not seeded"

        PendingHousekeeping ->
            "Pending housekeeping"

        HousekeepingInProgress ->
            "Housekeeping in progress"

        ReadyForUse ->
            "Ready for use"
