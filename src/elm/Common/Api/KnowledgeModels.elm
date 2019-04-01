module Common.Api.KnowledgeModels exposing
    ( deleteKnowledgeModel
    , deleteMigration
    , fetchPreview
    , getKnowledgeModel
    , getKnowledgeModels
    , getMigration
    , postKnowledgeModel
    , postMigration
    , postMigrationConflict
    , putKnowledgeModel
    , putVersion
    )

import Common.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet, jwtPost, jwtPut)
import Common.AppState exposing (AppState)
import Json.Encode as Encode exposing (Value)
import KMEditor.Common.Models exposing (KnowledgeModel, KnowledgeModelDetail, knowledgeModelDecoder, knowledgeModelDetailDecoder, knowledgeModelListDecoder)
import KMEditor.Common.Models.Entities
import KMEditor.Common.Models.Events exposing (Event, encodeEvent)
import KMEditor.Common.Models.Migration exposing (Migration, migrationDecoder)


getKnowledgeModels : AppState -> ToMsg (List KnowledgeModel) msg -> Cmd msg
getKnowledgeModels =
    jwtGet "/branches" knowledgeModelListDecoder


getKnowledgeModel : String -> AppState -> ToMsg KnowledgeModelDetail msg -> Cmd msg
getKnowledgeModel uuid =
    jwtGet ("/branches/" ++ uuid) knowledgeModelDetailDecoder


postKnowledgeModel : Value -> AppState -> ToMsg KnowledgeModel msg -> Cmd msg
postKnowledgeModel =
    jwtFetch "/branches" knowledgeModelDecoder


putKnowledgeModel : String -> String -> String -> List Event -> AppState -> ToMsg () msg -> Cmd msg
putKnowledgeModel uuid name kmId events =
    let
        body =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "kmId", Encode.string kmId )
                , ( "events", Encode.list encodeEvent events )
                ]
    in
    jwtPut ("/branches/" ++ uuid) body


deleteKnowledgeModel : String -> AppState -> ToMsg () msg -> Cmd msg
deleteKnowledgeModel uuid =
    jwtDelete ("/branches/" ++ uuid)


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


putVersion : String -> String -> Value -> AppState -> ToMsg () msg -> Cmd msg
putVersion kmUuid version =
    jwtPut ("/branches/" ++ kmUuid ++ "/versions/" ++ version)


getMigration : String -> AppState -> ToMsg Migration msg -> Cmd msg
getMigration uuid =
    jwtGet ("/branches/" ++ uuid ++ "/migrations/current") migrationDecoder


postMigration : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
postMigration uuid =
    jwtPost ("/branches/" ++ uuid ++ "/migrations/current")


postMigrationConflict : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
postMigrationConflict uuid =
    jwtPost ("/branches/" ++ uuid ++ "/migrations/current/conflict")


deleteMigration : String -> AppState -> ToMsg () msg -> Cmd msg
deleteMigration uuid =
    jwtDelete ("/branches/" ++ uuid ++ "/migrations/current")
