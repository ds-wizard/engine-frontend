module Shared.Data.BootstrapConfig.OwlConfig exposing (OwlConfig, decoder, default)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias OwlConfig =
    { enabled : Bool
    , name : Maybe String
    , organizationId : Maybe String
    , kmId : Maybe String
    , version : Maybe String
    , previousPackageId : Maybe String
    , rootElement : Maybe String
    }


default : OwlConfig
default =
    { enabled = False
    , name = Nothing
    , organizationId = Nothing
    , kmId = Nothing
    , version = Nothing
    , previousPackageId = Nothing
    , rootElement = Nothing
    }


decoder : Decoder OwlConfig
decoder =
    D.succeed OwlConfig
        |> D.required "enabled" D.bool
        |> D.required "name" (D.maybe D.string)
        |> D.required "organizationId" (D.maybe D.string)
        |> D.required "kmId" (D.maybe D.string)
        |> D.required "version" (D.maybe D.string)
        |> D.required "previousPackageId" (D.maybe D.string)
        |> D.required "rootElement" (D.maybe D.string)
