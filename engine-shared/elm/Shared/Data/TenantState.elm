module Shared.Data.TenantState exposing
    ( TenantState(..)
    , decoder
    , filterOptions
    , toReadableString
    )

import Gettext exposing (gettext)
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


filterOptions : { a | locale : Gettext.Locale } -> List ( String, String )
filterOptions appState =
    let
        toOption state =
            ( toString state, toReadableString appState state )
    in
    List.map toOption all


toReadableString : { a | locale : Gettext.Locale } -> TenantState -> String
toReadableString { locale } state =
    case state of
        NotSeeded ->
            gettext "Not seeded" locale

        PendingHousekeeping ->
            gettext "Pending housekeeping" locale

        HousekeepingInProgress ->
            gettext "Housekeeping in progress" locale

        ReadyForUse ->
            gettext "Ready for use" locale
