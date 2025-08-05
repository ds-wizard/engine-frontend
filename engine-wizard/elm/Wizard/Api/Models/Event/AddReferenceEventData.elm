module Wizard.Api.Models.Event.AddReferenceEventData exposing
    ( AddReferenceEventData(..)
    , decoder
    , encode
    , getEntityVisibleName
    , init
    , map
    , toReference
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.Event.AddReferenceCrossEventData as AddReferenceCrossEventData exposing (AddReferenceCrossEventData)
import Wizard.Api.Models.Event.AddReferenceResourcePageEventData as AddReferenceResourcePageEventData exposing (AddReferenceResourcePageEventData)
import Wizard.Api.Models.Event.AddReferenceURLEventData as AddReferenceURLEventData exposing (AddReferenceURLEventData)
import Wizard.Api.Models.KnowledgeModel.Reference exposing (Reference)


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
    ( "eventType", E.string "AddReferenceEvent" ) :: eventData


init : AddReferenceEventData
init =
    AddReferenceURLEvent AddReferenceURLEventData.init


toReference : String -> AddReferenceEventData -> Reference
toReference referenceUuid data =
    case data of
        AddReferenceResourcePageEvent eventData ->
            AddReferenceResourcePageEventData.toReference referenceUuid eventData

        AddReferenceURLEvent eventData ->
            AddReferenceURLEventData.toReference referenceUuid eventData

        AddReferenceCrossEvent eventData ->
            AddReferenceCrossEventData.toReference referenceUuid eventData


getEntityVisibleName : AddReferenceEventData -> Maybe String
getEntityVisibleName =
    map .resourcePageUuid (Just << .label) (Just << .targetUuid)


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
