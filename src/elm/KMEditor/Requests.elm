module KMEditor.Requests exposing
    ( deleteKnowledgeModel
    , deleteMigration
    , getBranch
    , getKnowledgeModel
    , getKnowledgeModelData
    , getKnowledgeModels
    , getLevels
    , getMetrics
    , getMigration
    , postEventsBulk
    , postForPreview
    , postKnowledgeModel
    , postMigration
    , postMigrationConflict
    , putBranch
    , putKnowledgeModelVersion
    )

import Auth.Models exposing (Session)
import Http
import Json.Encode as Encode exposing (Value)
import KMEditor.Common.Models as Models exposing (KnowledgeModel, knowledgeModelDecoder, knowledgeModelListDecoder)
import KMEditor.Common.Models.Entities as Editor exposing (KnowledgeModel, knowledgeModelDecoder)
import KMEditor.Common.Models.Events exposing (Event, encodeEvent)
import KMEditor.Common.Models.Migration exposing (Migration, migrationDecoder)
import Requests


getKnowledgeModel : String -> Session -> Http.Request Models.KnowledgeModel
getKnowledgeModel uuid session =
    Requests.get session ("/branches/" ++ uuid) Models.knowledgeModelDecoder


getBranch : String -> Session -> Http.Request Models.Branch
getBranch uuid session =
    Requests.get session ("/branches/" ++ uuid) Models.branchDecoder


putBranch : String -> String -> String -> List Event -> Session -> Http.Request String
putBranch uuid name kmId events session =
    let
        data =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "kmId", Encode.string kmId )
                , ( "events", Encode.list encodeEvent events )
                ]
    in
    Requests.put data session ("/branches/" ++ uuid)


getKnowledgeModels : Session -> Http.Request (List Models.KnowledgeModel)
getKnowledgeModels session =
    Requests.get session "/branches" knowledgeModelListDecoder


postKnowledgeModel : Session -> Value -> Http.Request Models.KnowledgeModel
postKnowledgeModel session knowledgeModel =
    Requests.postWithResponse knowledgeModel session "/branches" Models.knowledgeModelDecoder


deleteKnowledgeModel : String -> Session -> Http.Request String
deleteKnowledgeModel uuid session =
    Requests.delete session ("/branches/" ++ uuid)


putKnowledgeModelVersion : String -> String -> Value -> Session -> Http.Request String
putKnowledgeModelVersion kmUuid version data session =
    Requests.put data session ("/branches/" ++ kmUuid ++ "/versions/" ++ version)


getKnowledgeModelData : String -> Session -> Http.Request Editor.KnowledgeModel
getKnowledgeModelData uuid session =
    Requests.get session ("/branches/" ++ uuid ++ "/km") Editor.knowledgeModelDecoder


getMetrics : Session -> Http.Request (List Editor.Metric)
getMetrics session =
    Requests.get session "/metrics" Editor.metricListDecoder


getLevels : Session -> Http.Request (List Editor.Level)
getLevels session =
    Requests.get session "/levels" Editor.levelListDecoder


postEventsBulk : Session -> String -> Value -> Http.Request String
postEventsBulk session uuid data =
    Requests.post data session ("/branches/" ++ uuid ++ "/events/_bulk")


postMigration : Session -> String -> Value -> Http.Request String
postMigration session uuid data =
    Requests.post data session ("/branches/" ++ uuid ++ "/migrations/current")


getMigration : String -> Session -> Http.Request Migration
getMigration uuid session =
    Requests.get session ("/branches/" ++ uuid ++ "/migrations/current") migrationDecoder


postMigrationConflict : String -> Session -> Value -> Http.Request String
postMigrationConflict uuid session data =
    Requests.post data session ("/branches/" ++ uuid ++ "/migrations/current/conflict")


deleteMigration : String -> Session -> Http.Request String
deleteMigration uuid session =
    Requests.delete session ("/branches/" ++ uuid ++ "/migrations/current")


postForPreview : Maybe String -> List Event -> List String -> Session -> Http.Request KnowledgeModel
postForPreview packageId events tagUuids session =
    let
        data =
            Encode.object
                [ ( "packageId", packageId |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
                , ( "events", Encode.list encodeEvent events )
                , ( "tagUuids", Encode.list Encode.string tagUuids )
                ]
    in
    Requests.postWithResponse data session "/knowledge-models/preview" knowledgeModelDecoder
