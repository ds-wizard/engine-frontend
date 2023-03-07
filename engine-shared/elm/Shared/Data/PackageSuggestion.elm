module Shared.Data.PackageSuggestion exposing
    ( PackageSuggestion
    , decoder
    , fromPackage
    , getLatestPackageId
    , isSamePackage
    , packageIdAll
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Data.Package exposing (Package)
import Version exposing (Version)


type alias PackageSuggestion =
    { id : String
    , name : String
    , description : String
    , version : Version
    , versions : List Version
    }


decoder : Decoder PackageSuggestion
decoder =
    D.succeed PackageSuggestion
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "version" Version.decoder
        |> D.required "versions" (D.list Version.decoder)


fromPackage : Package -> List Version -> PackageSuggestion
fromPackage package packageVersions =
    { id = package.id
    , name = package.name
    , description = package.description
    , version = package.version
    , versions = packageVersions
    }


isSamePackage : String -> String -> Bool
isSamePackage packageId1 packageId2 =
    let
        ( mbOrgId1, mbKmId1 ) =
            getPackageIdValues packageId1

        ( mbOrgId2, mbKmId2 ) =
            getPackageIdValues packageId2
    in
    Maybe.isJust mbOrgId1 && mbOrgId1 == mbOrgId2 && Maybe.isJust mbKmId1 && mbKmId1 == mbKmId2


packageIdAll : String -> String
packageIdAll packageId =
    case getPackageIdValues packageId of
        ( Just orgId, Just kmId ) ->
            orgId ++ ":" ++ kmId ++ ":all"

        _ ->
            packageId


getPackageIdValues : String -> ( Maybe String, Maybe String )
getPackageIdValues packageId =
    case String.split ":" packageId of
        orgId :: kmId :: _ ->
            ( Just orgId, Just kmId )

        _ ->
            ( Nothing, Nothing )


getLatestVersion : PackageSuggestion -> Maybe Version
getLatestVersion =
    List.last << List.sortWith Version.compare << .versions


getLatestPackageId : PackageSuggestion -> Maybe String
getLatestPackageId packageSuggestion =
    case ( String.split ":" packageSuggestion.id, getLatestVersion packageSuggestion ) of
        ( orgId :: kmId :: _, Just latestVersion ) ->
            Just (orgId ++ ":" ++ kmId ++ ":" ++ Version.toString latestVersion)

        _ ->
            Nothing
