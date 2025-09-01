module Wizard.Api.Models.Event.EditReferenceEventData exposing
    ( EditReferenceEventData(..)
    , apply
    , decoder
    , encode
    , getEntityVisibleName
    , map
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.Event.EditReferenceCrossEventData as EditReferenceCrossEventData exposing (EditReferenceCrossEventData)
import Wizard.Api.Models.Event.EditReferenceResourcePageEventData as EditReferenceResourcePageEventData exposing (EditReferenceResourcePageEventData)
import Wizard.Api.Models.Event.EditReferenceURLEventData as EditReferenceURLEventData exposing (EditReferenceURLEventData)
import Wizard.Api.Models.Event.EventField as EventField
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference(..))


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
                , resourcePageUuid = EventField.getValueWithDefault eventData.resourcePageUuid (Reference.getResourcePageUuid reference)
                , annotations = EventField.getValueWithDefault eventData.annotations (Reference.getAnnotations reference)
                }

        EditReferenceURLEvent eventData ->
            URLReference
                { uuid = Reference.getUuid reference
                , url = EventField.getValueWithDefault eventData.url (Maybe.withDefault "" (Reference.getUrl reference))
                , label = EventField.getValueWithDefault eventData.label (Maybe.withDefault "" (Reference.getLabel reference))
                , annotations = EventField.getValueWithDefault eventData.annotations (Reference.getAnnotations reference)
                }

        EditReferenceCrossEvent eventData ->
            CrossReference
                { uuid = Reference.getUuid reference
                , targetUuid = EventField.getValueWithDefault eventData.targetUuid (Maybe.withDefault "" (Reference.getTargetUuid reference))
                , description = EventField.getValueWithDefault eventData.description (Maybe.withDefault "" (Reference.getDescription reference))
                , annotations = EventField.getValueWithDefault eventData.annotations (Reference.getAnnotations reference)
                }


getEntityVisibleName : EditReferenceEventData -> Maybe String
getEntityVisibleName reference =
    case reference of
        EditReferenceResourcePageEvent data ->
            Maybe.withDefault Nothing (EventField.getValue data.resourcePageUuid)

        EditReferenceURLEvent data ->
            EventField.getValue data.label

        EditReferenceCrossEvent data ->
            EventField.getValue data.targetUuid


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


squash : EditReferenceEventData -> EditReferenceEventData -> EditReferenceEventData
squash old new =
    case ( old, new ) of
        ( EditReferenceResourcePageEvent oldData, EditReferenceResourcePageEvent newData ) ->
            EditReferenceResourcePageEvent (EditReferenceResourcePageEventData.squash oldData newData)

        ( EditReferenceURLEvent oldData, EditReferenceURLEvent newData ) ->
            EditReferenceURLEvent (EditReferenceURLEventData.squash oldData newData)

        ( EditReferenceCrossEvent oldData, EditReferenceCrossEvent newData ) ->
            EditReferenceCrossEvent (EditReferenceCrossEventData.squash oldData newData)

        _ ->
            new
