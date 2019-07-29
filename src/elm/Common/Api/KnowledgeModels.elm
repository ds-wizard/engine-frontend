module Common.Api.KnowledgeModels exposing (fetchPreview)

import Common.Api exposing (ToMsg, jwtFetch)
import Common.AppState exposing (AppState)
import Json.Encode as Encode exposing (Value)
import KMEditor.Common.Models.Entities
import KMEditor.Common.Models.Events exposing (Event, encodeEvent)


fetchPreview : Maybe String -> List Event -> List String -> AppState -> ToMsg KMEditor.Common.Models.Entities.KnowledgeModel msg -> Cmd msg
fetchPreview packageId events tagUuids =
    let
        data =
            Encode.object
                [ ( "packageId", packageId |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
                , ( "events", Encode.list encodeEvent events )
                , ( "tagUuids", Encode.list Encode.string tagUuids )
                ]
    in
    jwtFetch "/knowledge-models/preview" KMEditor.Common.Models.Entities.knowledgeModelDecoder data
