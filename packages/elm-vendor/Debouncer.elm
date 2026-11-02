module Debouncer exposing
    ( Debouncer
    , DebouncerConfig
    , Msg
    , UpdateConfig
    , debounce
    , provideInput
    , toDebouncer
    , update
    )

{-| A debounce: a message is passed on once no other has been provided for the
configured time, and only the last one is.

Each input starts a timer that carries the input's number. When the timer goes
off, the input is passed on if no later one came; otherwise it is dropped and
the later input's own timer decides. Nothing compares clock readings, so a timer
that fires a little early by the wall clock — which a busy event loop does —
cannot lose the last input.

-}

import Process
import Task


type Debouncer msg
    = Debouncer { interval : Float, latest : Int }


type DebouncerConfig msg
    = DebouncerConfig Float


debounce : Int -> DebouncerConfig msg
debounce interval =
    DebouncerConfig (toFloat interval)


toDebouncer : DebouncerConfig msg -> Debouncer msg
toDebouncer (DebouncerConfig interval) =
    Debouncer { interval = interval, latest = 0 }


type Msg msg
    = ProvideInput msg
    | Settle Int msg


provideInput : msg -> Msg msg
provideInput =
    ProvideInput


type alias UpdateConfig msgA msgB model =
    { mapMsg : Msg msgA -> msgB
    , getDebouncer : model -> Debouncer msgA
    , setDebouncer : Debouncer msgA -> model -> model
    }


update :
    (msgA -> model -> ( model, Cmd msgB ))
    -> UpdateConfig msgA msgB model
    -> Msg msgA
    -> model
    -> ( model, Cmd msgB )
update parentUpdate config msg model =
    let
        (Debouncer debouncer) =
            config.getDebouncer model
    in
    case msg of
        ProvideInput input ->
            let
                latest =
                    debouncer.latest + 1
            in
            ( config.setDebouncer (Debouncer { debouncer | latest = latest }) model
            , Process.sleep debouncer.interval
                |> Task.perform (\_ -> config.mapMsg (Settle latest input))
            )

        Settle number input ->
            if number == debouncer.latest then
                parentUpdate input model

            else
                ( model, Cmd.none )
