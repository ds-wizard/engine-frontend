module Common.Plugins.IdentifierPattern exposing (IdentifierPattern, decoder, fromString, matches)

{-| A pattern a plugin names knowledge models or document templates with, and
matching it against what a thing is identified by: its id and its version,
`dsw.root:2.4.0`.

A pattern is an id and a version pattern, `dsw.root:^2`, with `*` for any id.

Plugins written when a thing had an organization id and an id of its own name
them in three parts, `dsw:root:^2` or `dsw:*:^2`. Such a pattern still works:
the two parts are the id they were joined into, `dsw.root`, and `dsw:*` is every
id that starts with `dsw.`.

-}

import Json.Decode as D
import Version
import VersionPattern exposing (VersionPattern)


type IdentifierPattern
    = IdentifierPattern IdentifierPatternData


type alias IdentifierPatternData =
    { idPattern : IdPattern
    , versionPattern : VersionPattern
    }


type IdPattern
    = AnyId
    | ExactId String
    | IdWithPrefix String


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
    let
        withVersion idPattern verPart =
            VersionPattern.fromString verPart
                |> Result.toMaybe
                |> Maybe.map (\versionPattern -> IdentifierPattern { idPattern = idPattern, versionPattern = versionPattern })
    in
    case String.split ":" str of
        [ "*", verPart ] ->
            withVersion AnyId verPart

        [ id, verPart ] ->
            withVersion (ExactId id) verPart

        [ orgId, "*", verPart ] ->
            withVersion (IdWithPrefix (orgId ++ ".")) verPart

        [ orgId, compPart, verPart ] ->
            withVersion (ExactId (orgId ++ "." ++ compPart)) verPart

        _ ->
            Nothing


matches : IdentifierPattern -> String -> Bool
matches (IdentifierPattern data) identifierStr =
    case String.split ":" identifierStr of
        [ id, verPart ] ->
            let
                idMatch =
                    case data.idPattern of
                        AnyId ->
                            True

                        ExactId exact ->
                            exact == id

                        IdWithPrefix prefix ->
                            String.startsWith prefix id

                versionMatch =
                    case Version.fromString verPart of
                        Just version ->
                            VersionPattern.matches data.versionPattern version

                        Nothing ->
                            False
            in
            idMatch && versionMatch

        _ ->
            False
