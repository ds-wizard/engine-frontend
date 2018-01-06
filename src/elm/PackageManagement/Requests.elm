module PackageManagement.Requests exposing (..)

{-|


# Package requests

@docs getPackagesUnique, getPackages, getPackagesFiltered, deletePackage, deletePackageVersion


# Import / Export

@docs importPackage, exportPackageUrl

-}

import Auth.Models exposing (Session)
import FileReader exposing (NativeFile)
import Http
import Json.Decode as Decode
import Jwt
import PackageManagement.Models exposing (Package, PackageDetail, packageDetailListDecoder, packageListDecoder)
import Requests exposing (apiUrl)


{-| -}
getPackagesUnique : Session -> Http.Request (List Package)
getPackagesUnique session =
    Requests.get session "/packages/unique" packageListDecoder


{-| -}
getPackages : Session -> Http.Request (List PackageDetail)
getPackages session =
    Requests.get session "/packages" packageDetailListDecoder


{-| -}
getPackagesFiltered : String -> String -> Session -> Http.Request (List PackageDetail)
getPackagesFiltered groupId artifactId session =
    Requests.get session ("/packages/?groupId=" ++ groupId ++ "&artifactId=" ++ artifactId) packageDetailListDecoder


{-| -}
deletePackage : String -> String -> Session -> Http.Request String
deletePackage groupId artifactId session =
    Requests.delete session ("/packages/?groupId=" ++ groupId ++ "&artifactId=" ++ artifactId)


{-| -}
deletePackageVersion : String -> Session -> Http.Request String
deletePackageVersion packageId session =
    Requests.delete session ("/packages/" ++ packageId)


{-| -}
importPackage : NativeFile -> Session -> Http.Request Decode.Value
importPackage file session =
    let
        body =
            Http.multipartBody [ FileReader.filePart "file" file ]
    in
    Jwt.post session.token (apiUrl "/import") body Decode.value


{-| -}
exportPackageUrl : String -> String
exportPackageUrl packageId =
    apiUrl <| "/export/" ++ packageId
