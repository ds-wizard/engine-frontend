module Wizard.KMEditor.Common.Events.EventField exposing
    ( EventField
    , create
    , decoder
    , empty
    , encode
    , getValue
    , getValueWithDefault
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias EventField a =
    { changed : Bool
    , value : Maybe a
    }


decoder : Decoder a -> Decoder (EventField a)
decoder valueDecoder =
    D.succeed EventField
        |> D.required "changed" D.bool
        |> D.optional "value" (D.nullable valueDecoder) Nothing


encode : (a -> E.Value) -> EventField a -> E.Value
encode encodeValue field =
    case ( field.changed, field.value ) of
        ( True, Just value ) ->
            E.object
                [ ( "changed", E.bool True )
                , ( "value", encodeValue value )
                ]

        ( True, Nothing ) ->
            E.object
                [ ( "changed", E.bool True )
                , ( "value", E.null )
                ]

        _ ->
            E.object
                [ ( "changed", E.bool False )
                ]


empty : EventField a
empty =
    { changed = False
    , value = Nothing
    }


create : a -> Bool -> EventField a
create value changed =
    let
        v =
            if changed then
                Just value

            else
                Nothing
    in
    { changed = changed
    , value = v
    }


getValue : EventField a -> Maybe a
getValue eventField =
    if eventField.changed then
        eventField.value

    else
        Nothing


getValueWithDefault : EventField a -> a -> a
getValueWithDefault eventField default =
    getValue eventField |> Maybe.withDefault default
