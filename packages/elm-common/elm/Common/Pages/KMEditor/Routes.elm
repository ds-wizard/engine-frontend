module Common.Pages.KMEditor.Routes exposing
    ( Route(..)
    , isAllowed
    , parsers
    , toUrl
    )

import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Pages.KMEditor.Editor.Routes as KMEditorRoute exposing (KMEditorRoute)
import Flip exposing (flip)
import Url.Parser exposing ((</>), (<?>), Parser, map, s)
import Url.Parser.Extensions exposing (uuid)
import Url.Parser.Query.Extensions as Query
import Uuid exposing (Uuid)


type Route
    = CreateRoute (Maybe Uuid) (Maybe Bool)
    | EditorRoute Uuid KMEditorRoute
    | IndexRoute PaginationQueryString
    | MigrationRoute Uuid
    | PublishRoute Uuid


{-| The editors live under the knowledge models they make.
-}
moduleRoot : List String
moduleRoot =
    [ "knowledge-models", "editors" ]


root : Parser a a
root =
    s "knowledge-models" </> s "editors"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        editorWithEntityRoute editorUuid entityUuid =
            wrapRoute <| EditorRoute editorUuid <| KMEditorRoute.Edit (Just entityUuid)
    in
    [ map (createRoute wrapRoute) (root </> s "create" <?> Query.uuid "selected" <?> Query.bool "edit")
    , map (wrapRoute << flip EditorRoute (KMEditorRoute.Edit Nothing)) (root </> uuid)
    , map editorWithEntityRoute (root </> uuid </> s "edit" </> uuid)
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Phases) (root </> uuid </> s "phases")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.QuestionTags) (root </> uuid </> s "question-tags")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Preview) (root </> uuid </> s "preview")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Settings) (root </> uuid </> s "settings")
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "updatedAt,desc")) (PaginationQueryString.parser root)
    , map (wrapRoute << MigrationRoute) (root </> uuid </> s "migration")
    , map (wrapRoute << PublishRoute) (root </> uuid </> s "publish")
    ]


createRoute : (Route -> a) -> Maybe Uuid -> Maybe Bool -> a
createRoute wrapRoute kmUuid edit =
    wrapRoute <| CreateRoute kmUuid edit


toUrl : Route -> List String
toUrl route =
    case route of
        CreateRoute mbSelected mbEdit ->
            case ( mbSelected, mbEdit ) of
                ( Just kmUuid, Just edit ) ->
                    let
                        editString =
                            if edit then
                                "true"

                            else
                                "false"
                    in
                    moduleRoot
                        ++ [ "create"
                           , "?selected=" ++ Uuid.toString kmUuid ++ "&edit=" ++ editString
                           ]

                ( Just kmUuid, Nothing ) ->
                    moduleRoot
                        ++ [ "create"
                           , "?selected=" ++ Uuid.toString kmUuid
                           ]

                _ ->
                    moduleRoot ++ [ "create" ]

        EditorRoute uuid subroute ->
            let
                base =
                    moduleRoot ++ [ Uuid.toString uuid ]
            in
            case subroute of
                KMEditorRoute.Edit mbUuid ->
                    case mbUuid of
                        Just entityUuid ->
                            base ++ [ "edit", Uuid.toString entityUuid ]

                        Nothing ->
                            base

                KMEditorRoute.Phases ->
                    base ++ [ "phases" ]

                KMEditorRoute.QuestionTags ->
                    base ++ [ "question-tags" ]

                KMEditorRoute.Preview ->
                    base ++ [ "preview" ]

                KMEditorRoute.Settings ->
                    base ++ [ "settings" ]

        IndexRoute paginationQueryString ->
            [ "knowledge-models", "editors" ++ PaginationQueryString.toUrl paginationQueryString ]

        MigrationRoute uuid ->
            moduleRoot ++ [ Uuid.toString uuid, "migration" ]

        PublishRoute uuid ->
            moduleRoot ++ [ Uuid.toString uuid, "publish" ]


isAllowed : Route -> AppStateLike a -> Bool
isAllowed route appState =
    case route of
        CreateRoute _ _ ->
            Feature.knowledgeModelEditorsCreate appState

        EditorRoute _ _ ->
            Feature.knowledgeModelEditorsEdit appState

        IndexRoute _ ->
            Feature.knowledgeModelEditorsView appState

        MigrationRoute _ ->
            Feature.knowledgeModelEditorsUpgrade appState

        PublishRoute _ ->
            Feature.knowledgeModelEditorsPublish appState
