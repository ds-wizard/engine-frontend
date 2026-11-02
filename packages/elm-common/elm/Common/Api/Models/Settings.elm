module Common.Api.Models.Settings exposing
    ( Settings
    , State(..)
    , decoder
    , encode
    , state
    )

{-| What every settings section is read and written in: the section's value and
two facts about overriding it.

Both facts are `Nothing` where they do not apply, so the shape is the same for
every section and wherever it is asked for:

  - `overrideAllowed` — whether the section may be given a value of its own
    below where the defaults are kept.
  - `overridden` — whether the value read is such an own value. It is only
    known where a value can be overridden.

A section is written back as it was read, with `value` changed; the server
ignores what cannot be changed from where the request is made.

-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias Settings value =
    { value : value
    , overrideAllowed : Maybe Bool
    , overridden : Maybe Bool
    }


{-| What the two facts mean together for whoever is editing the section.
-}
type State
    = -- A value nothing overrides and nothing inherits.
      Plain
      -- The defaults, and whether they may be overridden.
    | Defaults Bool
      -- A place that uses the defaults and may replace them.
    | Inherited
      -- A place with a value of its own.
    | Overridden
      -- A place that uses the defaults and may not replace them.
    | Locked


state : Settings value -> State
state settings =
    case ( settings.overridden, settings.overrideAllowed ) of
        ( Nothing, Nothing ) ->
            Plain

        ( Nothing, Just allowed ) ->
            Defaults allowed

        ( Just _, Just False ) ->
            Locked

        ( Just True, _ ) ->
            Overridden

        ( Just False, _ ) ->
            Inherited


decoder : Decoder value -> Decoder (Settings value)
decoder valueDecoder =
    D.succeed Settings
        |> D.required "value" valueDecoder
        |> D.required "overrideAllowed" (D.nullable D.bool)
        |> D.required "overridden" (D.nullable D.bool)


encode : (value -> E.Value) -> Settings value -> E.Value
encode encodeValue settings =
    E.object
        [ ( "value", encodeValue settings.value )
        , ( "overrideAllowed", E.maybe E.bool settings.overrideAllowed )
        , ( "overridden", E.maybe E.bool settings.overridden )
        ]
