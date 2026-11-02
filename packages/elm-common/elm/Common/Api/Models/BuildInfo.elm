module Common.Api.Models.BuildInfo exposing (BuildInfo, MetamodelVersionInfo, client, clientReleaseVersion, decoder)

import Common.Api.Models.BuildInfo.BuildInfoComponent as BuildInfoComponent exposing (BuildInfoComponent)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe


type alias BuildInfo =
    { version : String
    , builtAt : String
    , components : List BuildInfoComponent
    , metamodelVersions : Maybe (List MetamodelVersionInfo)
    }


type alias MetamodelVersionInfo =
    { name : String
    , version : String
    }


client : BuildInfo
client =
    { version = "{version}"
    , builtAt = "{builtAt}"
    , components = []
    , metamodelVersions = Nothing
    }


decoder : Decoder BuildInfo
decoder =
    D.succeed BuildInfo
        |> D.required "version" D.string
        |> D.required "builtAt" D.string
        |> D.required "components" (D.list BuildInfoComponent.decoder)
        |> D.optional "metamodelVersions" (D.maybe (D.list metamodelVersionInfoDecoder)) Nothing


metamodelVersionInfoDecoder : Decoder MetamodelVersionInfo
metamodelVersionInfoDecoder =
    D.succeed MetamodelVersionInfo
        |> D.required "name" D.string
        |> D.required "version" D.string


{-| The client's own release, `5.0.0` of a build of the tag `v5.0.0`; nothing for
a build of a branch (`develop~1a2b3c4`), which is no release.
-}
clientReleaseVersion : Maybe String
clientReleaseVersion =
    let
        tag =
            Maybe.withDefault "" (List.head (String.split "~" client.version))

        version =
            if String.startsWith "v" tag then
                String.dropLeft 1 tag

            else
                tag

        isNumber part =
            Maybe.isJust (String.toInt part)
    in
    case String.split "." (Maybe.withDefault "" (List.head (String.split "-" version))) of
        [ major, minor, patch ] ->
            if List.all isNumber [ major, minor, patch ] then
                Just version

            else
                Nothing

        _ ->
            Nothing
