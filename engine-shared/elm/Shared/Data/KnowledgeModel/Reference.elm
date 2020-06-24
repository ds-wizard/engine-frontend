module Shared.Data.KnowledgeModel.Reference exposing
    ( Reference(..)
    , decoder
    , getUuid
    , getVisibleName
    , map
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Shared.Data.KnowledgeModel.Reference.CrossReferenceData as CrossReferenceData exposing (CrossReferenceData)
import Shared.Data.KnowledgeModel.Reference.ReferenceType as ReferenceType exposing (ReferenceType(..))
import Shared.Data.KnowledgeModel.Reference.ResourcePageReferenceData as ResourcePageReferenceData exposing (ResourcePageReferenceData)
import Shared.Data.KnowledgeModel.Reference.URLReferenceData as URLReferenceData exposing (URLReferenceData)


type Reference
    = ResourcePageReference ResourcePageReferenceData
    | URLReference URLReferenceData
    | CrossReference CrossReferenceData


new : String -> Reference
new =
    URLReference << URLReferenceData.new



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


map : (ResourcePageReferenceData -> a) -> (URLReferenceData -> a) -> (CrossReferenceData -> a) -> Reference -> a
map resourcePageReference urlReference crossReference reference =
    case reference of
        ResourcePageReference data ->
            resourcePageReference data

        URLReference data ->
            urlReference data

        CrossReference data ->
            crossReference data


getUuid : Reference -> String
getUuid =
    map .uuid .uuid .uuid


getVisibleName : Reference -> String
getVisibleName =
    map .shortUuid .label .targetUuid
