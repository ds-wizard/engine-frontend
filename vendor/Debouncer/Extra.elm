module Debouncer.Extra exposing
    ( Debouncer
    , DebouncerConfig
    , Msg
    , UpdateConfig
    , debounce
    , provideInput
    , toDebouncer
    , update
    )

import Debouncer.Basic


type alias Debouncer msg =
    Debouncer.Basic.Debouncer msg msg


type alias DebouncerConfig msg =
    Debouncer.Basic.Config msg msg


debounce : Int -> DebouncerConfig msg
debounce =
    Debouncer.Basic.debounce


toDebouncer : DebouncerConfig msg -> Debouncer msg
toDebouncer =
    Debouncer.Basic.toDebouncer


type alias Msg msg =
    Debouncer.Basic.Msg msg


provideInput : msg -> Msg msg
provideInput =
    Debouncer.Basic.provideInput


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
        ( updatedDebouncer, cmd, output ) =
            Debouncer.Basic.update msg (config.getDebouncer model)

        mappedCmd =
            Cmd.map config.mapMsg cmd

        newModel =
            config.setDebouncer updatedDebouncer model
    in
    output
        |> Maybe.map
            (\emittedMsg ->
                parentUpdate emittedMsg newModel
                    |> Tuple.mapSecond (\recursiveCmd -> Cmd.batch [ mappedCmd, recursiveCmd ])
            )
        |> Maybe.withDefault
            ( newModel, mappedCmd )
