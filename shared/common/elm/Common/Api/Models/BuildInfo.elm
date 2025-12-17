module Common.Api.Models.BuildInfo exposing (BuildInfo, MetamodelVersionInfo, client, decoder)

import Common.Api.Models.BuildInfo.BuildInfoComponent as BuildInfoComponent exposing (BuildInfoComponent)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


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
