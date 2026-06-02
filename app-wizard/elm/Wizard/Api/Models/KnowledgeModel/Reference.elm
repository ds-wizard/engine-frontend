module Wizard.Api.Models.KnowledgeModel.Reference exposing
    ( Reference(..)
    , decoder
    , equalContent
    , getAnnotations
    , getDescription
    , getLabel
    , getResourcePageUuid
    , getTargetUuid
    , getTypeReadableString
    , getTypeString
    , getUrl
    , getUuid
    , getVisibleName
    , map
    )

import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Wizard.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Reference.CrossReferenceData as CrossReferenceData exposing (CrossReferenceData)
import Wizard.Api.Models.KnowledgeModel.Reference.ReferenceType as ReferenceType exposing (ReferenceType(..))
import Wizard.Api.Models.KnowledgeModel.Reference.ResourcePageReferenceData as ResourcePageReferenceData exposing (ResourcePageReferenceData)
import Wizard.Api.Models.KnowledgeModel.Reference.URLReferenceData as URLReferenceData exposing (URLReferenceData)
import Wizard.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)


type Reference
    = ResourcePageReference ResourcePageReferenceData
    | URLReference URLReferenceData
    | CrossReference CrossReferenceData



-- Decoders


decoder : Decoder Reference
decoder =
    D.oneOf
        [ D.when ReferenceType.decoder ((==) ResourcePageReferenceType) resourcePageReferenceDecoder
        , D.when ReferenceType.decoder ((==) URLReferenceType) urlReferenceDecoder
        , D.when ReferenceType.decoder ((==) CrossReferenceType) crossReferenceDecoder
        ]


resourcePageReferenceDecoder : Decoder Reference
resourcePageReferenceDecoder =
    D.map ResourcePageReference ResourcePageReferenceData.decoder


urlReferenceDecoder : Decoder Reference
urlReferenceDecoder =
    D.map URLReference URLReferenceData.decoder


crossReferenceDecoder : Decoder Reference
crossReferenceDecoder =
    D.map CrossReference CrossReferenceData.decoder



-- Helpers


equalContent : Reference -> Reference -> Bool
equalContent reference1 reference2 =
    case ( reference1, reference2 ) of
        ( ResourcePageReference data1, ResourcePageReference data2 ) ->
            ResourcePageReferenceData.equalContent data1 data2

        ( URLReference data1, URLReference data2 ) ->
            URLReferenceData.equalContent data1 data2

        ( CrossReference data1, CrossReference data2 ) ->
            CrossReferenceData.equalContent data1 data2

        _ ->
            False


map : (ResourcePageReferenceData -> a) -> (URLReferenceData -> a) -> (CrossReferenceData -> a) -> Reference -> a
map resourcePageReference urlReference crossReference reference =
    case reference of
        ResourcePageReference data ->
            resourcePageReference data

        URLReference data ->
            urlReference data

        CrossReference data ->
            crossReference data


getTypeString : Reference -> String
getTypeString =
    map (always "ResourcePage") (always "URL") (always "Cross")


getTypeReadableString : Gettext.Locale -> Reference -> String
getTypeReadableString locale =
    map
        (always <| gettext "Resource Page" locale)
        (always <| gettext "URL" locale)
        (always <| gettext "Cross Reference" locale)


getUuid : Reference -> String
getUuid =
    map .uuid .uuid .uuid


getVisibleName : List Question -> List ResourcePage -> Reference -> String
getVisibleName questions resourcePages =
    map
        (ResourcePageReferenceData.toLabel resourcePages)
        URLReferenceData.toLabel
        (CrossReferenceData.toLabel questions)


getAnnotations : Reference -> List Annotation
getAnnotations =
    map .annotations .annotations .annotations


getResourcePageUuid : Reference -> Maybe String
getResourcePageUuid =
    map .resourcePageUuid (always Nothing) (always Nothing)


getUrl : Reference -> Maybe String
getUrl =
    map (always Nothing) (Just << .url) (always Nothing)


getLabel : Reference -> Maybe String
getLabel =
    map (always Nothing) (Just << .label) (always Nothing)


getTargetUuid : Reference -> Maybe String
getTargetUuid =
    map (always Nothing) (always Nothing) (Just << .targetUuid)


getDescription : Reference -> Maybe String
getDescription =
    map (always Nothing) (always Nothing) (Just << .description)
