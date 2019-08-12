module KMEditor.Common.Events.AddReferenceEventData exposing
    ( AddReferenceEventData(..)
    , decoder
    , encode
    , getEntityVisibleName
    , map
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import KMEditor.Common.Events.AddReferenceCrossEventData as AddReferenceCrossEventData exposing (AddReferenceCrossEventData)
import KMEditor.Common.Events.AddReferenceResourcePageEventData as AddReferenceResourcePageEventData exposing (AddReferenceResourcePageEventData)
import KMEditor.Common.Events.AddReferenceURLEventData as AddReferenceURLEventData exposing (AddReferenceURLEventData)


type AddReferenceEventData
    = AddReferenceResourcePageEvent AddReferenceResourcePageEventData
    | AddReferenceURLEvent AddReferenceURLEventData
    | AddReferenceCrossEvent AddReferenceCrossEventData


decoder : Decoder AddReferenceEventData
decoder =
    D.field "referenceType" D.string
        |> D.andThen
            (\referenceType ->
                case referenceType of
                    "ResourcePageReference" ->
                        D.map AddReferenceResourcePageEvent AddReferenceResourcePageEventData.decoder

                    "URLReference" ->
                        D.map AddReferenceURLEvent AddReferenceURLEventData.decoder

                    "CrossReference" ->
                        D.map AddReferenceCrossEvent AddReferenceCrossEventData.decoder

                    _ ->
                        D.fail <| "Unknown reference type: " ++ referenceType
            )


encode : AddReferenceEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                AddReferenceResourcePageEventData.encode
                AddReferenceURLEventData.encode
                AddReferenceCrossEventData.encode
                data
    in
    [ ( "eventType", E.string "AddReferenceEvent" ) ] ++ eventData


getEntityVisibleName : AddReferenceEventData -> Maybe String
getEntityVisibleName =
    Just << map .shortUuid .label .targetUuid


map :
    (AddReferenceResourcePageEventData -> a)
    -> (AddReferenceURLEventData -> a)
    -> (AddReferenceCrossEventData -> a)
    -> AddReferenceEventData
    -> a
map resourcePageReference urlReference crossReference reference =
    case reference of
        AddReferenceResourcePageEvent data ->
            resourcePageReference data

        AddReferenceURLEvent data ->
            urlReference data

        AddReferenceCrossEvent data ->
            crossReference data
