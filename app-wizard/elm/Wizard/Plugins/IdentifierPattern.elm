module Wizard.Plugins.IdentifierPattern exposing (IdentifierPattern, decoder, matches)

import Json.Decode as D
import Version
import VersionPattern exposing (VersionPattern)


type IdentifierPattern
    = IdentifierPattern IdentifierPatternData


type alias IdentifierPatternData =
    { orgId : String
    , componentPattern : ComponentPattern
    , versionPattern : VersionPattern
    }


type ComponentPattern
    = ComponentAny
    | ComponentSpecific String


decoder : D.Decoder IdentifierPattern
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just pattern ->
                        D.succeed pattern

                    Nothing ->
                        D.fail ("Invalid IdentifierPattern: " ++ str)
            )


fromString : String -> Maybe IdentifierPattern
fromString str =
    case String.split ":" str of
        [ orgId, compPart, verPart ] ->
            case VersionPattern.fromString verPart of
                Ok versionPattern ->
                    let
                        componentPattern =
                            case compPart of
                                "*" ->
                                    ComponentAny

                                specific ->
                                    ComponentSpecific specific
                    in
                    Just <|
                        IdentifierPattern
                            { orgId = orgId
                            , componentPattern = componentPattern
                            , versionPattern = versionPattern
                            }

                _ ->
                    Nothing

        _ ->
            Nothing


matches : IdentifierPattern -> String -> Bool
matches (IdentifierPattern data) identifierStr =
    case String.split ":" identifierStr of
        [ orgId, compPart, verPart ] ->
            let
                orgIdMatch =
                    data.orgId == orgId

                componentMatch =
                    case data.componentPattern of
                        ComponentAny ->
                            True

                        ComponentSpecific specific ->
                            specific == compPart

                versionMatch =
                    case Version.fromString verPart of
                        Just version ->
                            VersionPattern.matches data.versionPattern version

                        Nothing ->
                            False
            in
            orgIdMatch && componentMatch && versionMatch

        _ ->
            False
