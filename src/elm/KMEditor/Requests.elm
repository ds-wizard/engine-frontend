module KMEditor.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Encode as Encode exposing (Value)
import KMEditor.Common.Models as Models exposing (KnowledgeModel, knowledgeModelDecoder, knowledgeModelListDecoder)
import KMEditor.Common.Models.Entities as Editor exposing (KnowledgeModel, knowledgeModelDecoder)
import KMEditor.Common.Models.Migration exposing (Migration, migrationDecoder)
import Requests


getKnowledgeModel : String -> Session -> Http.Request Models.KnowledgeModel
getKnowledgeModel uuid session =
    Requests.get session ("/branches/" ++ uuid) Models.knowledgeModelDecoder


getKnowledgeModels : Session -> Http.Request (List Models.KnowledgeModel)
getKnowledgeModels session =
    Requests.get session "/branches" knowledgeModelListDecoder


postKnowledgeModel : Session -> Value -> Http.Request String
postKnowledgeModel session knowledgeModel =
    Requests.post knowledgeModel session "/branches"


deleteKnowledgeModel : String -> Session -> Http.Request String
deleteKnowledgeModel uuid session =
    Requests.delete session ("/branches/" ++ uuid)


putKnowledgeModelVersion : String -> String -> Value -> Session -> Http.Request String
putKnowledgeModelVersion kmUuid version data session =
    Requests.put data session ("/branches/" ++ kmUuid ++ "/versions/" ++ version)


getKnowledgeModelData : String -> Session -> Http.Request Editor.KnowledgeModel
getKnowledgeModelData uuid session =
    Requests.get session ("/branches/" ++ uuid ++ "/km") Editor.knowledgeModelDecoder


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
