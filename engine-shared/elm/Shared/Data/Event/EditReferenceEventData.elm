module Shared.Data.Event.EditReferenceEventData exposing
    ( EditReferenceEventData(..)
    , apply
    , decoder
    , encode
    , getEntityVisibleName
    , map
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Shared.Data.Event.EditReferenceCrossEventData as EditReferenceCrossEventData exposing (EditReferenceCrossEventData)
import Shared.Data.Event.EditReferenceResourcePageEventData as EditReferenceResourcePageEventData exposing (EditReferenceResourcePageEventData)
import Shared.Data.Event.EditReferenceURLEventData as EditReferenceURLEventData exposing (EditReferenceURLEventData)
import Shared.Data.Event.EventField as EventField
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference(..))


type EditReferenceEventData
    = EditReferenceResourcePageEvent EditReferenceResourcePageEventData
    | EditReferenceURLEvent EditReferenceURLEventData
    | EditReferenceCrossEvent EditReferenceCrossEventData


decoder : Decoder EditReferenceEventData
decoder =
    D.field "referenceType" D.string
        |> D.andThen
            (\referenceType ->
                case referenceType of
                    "ResourcePageReference" ->
                        D.map EditReferenceResourcePageEvent EditReferenceResourcePageEventData.decoder

                    "URLReference" ->
                        D.map EditReferenceURLEvent EditReferenceURLEventData.decoder

                    "CrossReference" ->
                        D.map EditReferenceCrossEvent EditReferenceCrossEventData.decoder

                    _ ->
                        D.fail <| "Unknown reference type: " ++ referenceType
            )


encode : EditReferenceEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                EditReferenceResourcePageEventData.encode
                EditReferenceURLEventData.encode
                EditReferenceCrossEventData.encode
                data
    in
    ( "eventType", E.string "EditReferenceEvent" ) :: eventData


apply : EditReferenceEventData -> Reference -> Reference
apply event reference =
    case event of
        EditReferenceResourcePageEvent eventData ->
            ResourcePageReference
                { uuid = Reference.getUuid reference
                , shortUuid = EventField.getValueWithDefault eventData.shortUuid (Maybe.withDefault "" (Reference.getShortUuid reference))
                , annotations = Reference.getAnnotations reference
                }

        EditReferenceURLEvent eventData ->
            URLReference
                { uuid = Reference.getUuid reference
                , url = EventField.getValueWithDefault eventData.url (Maybe.withDefault "" (Reference.getUrl reference))
                , label = EventField.getValueWithDefault eventData.label (Maybe.withDefault "" (Reference.getLabel reference))
                , annotations = Reference.getAnnotations reference
                }

        EditReferenceCrossEvent eventData ->
            CrossReference
                { uuid = Reference.getUuid reference
                , targetUuid = EventField.getValueWithDefault eventData.targetUuid (Maybe.withDefault "" (Reference.getTargetUuid reference))
                , description = EventField.getValueWithDefault eventData.description (Maybe.withDefault "" (Reference.getDescription reference))
                , annotations = Reference.getAnnotations reference
                }


getEntityVisibleName : EditReferenceEventData -> Maybe String
getEntityVisibleName =
    EventField.getValue << map .shortUuid .label .targetUuid


map :
    (EditReferenceResourcePageEventData -> a)
    -> (EditReferenceURLEventData -> a)
    -> (EditReferenceCrossEventData -> a)
    -> EditReferenceEventData
    -> a
map resourcePageReference urlReference crossReference reference =
    case reference of
        EditReferenceResourcePageEvent data ->
            resourcePageReference data

        EditReferenceURLEvent data ->
            urlReference data

        EditReferenceCrossEvent data ->
            crossReference data
