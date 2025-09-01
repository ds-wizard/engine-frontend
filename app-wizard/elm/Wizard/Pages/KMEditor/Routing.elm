module Wizard.Pages.KMEditor.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Flip exposing (flip)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing ((</>), (<?>), Parser, map, s)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Url.Parser.Query.Extra as Query
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.Pages.KMEditor.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "knowledge-model-editors"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        editorWithEntityRoute editorUuid entityUuid =
            wrapRoute <| EditorRoute editorUuid <| KMEditorRoute.Edit (Just entityUuid)
    in
    [ map (createRoute wrapRoute) (s moduleRoot </> s "create" <?> Query.string "selected" <?> Query.bool "edit")
    , map (wrapRoute << flip EditorRoute (KMEditorRoute.Edit Nothing)) (s moduleRoot </> s "editor" </> uuid)
    , map editorWithEntityRoute (s moduleRoot </> s "editor" </> uuid </> s "edit" </> uuid)
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Phases) (s moduleRoot </> s "editor" </> uuid </> s "phases")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.QuestionTags) (s moduleRoot </> s "editor" </> uuid </> s "question-tags")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Preview) (s moduleRoot </> s "editor" </> uuid </> s "preview")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Settings) (s moduleRoot </> s "editor" </> uuid </> s "settings")
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "updatedAt,desc")) (PaginationQueryString.parser (s moduleRoot))
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s "migration" </> uuid)
    , map (wrapRoute << PublishRoute) (s moduleRoot </> s "publish" </> uuid)
    ]


createRoute : (Route -> a) -> Maybe String -> Maybe Bool -> a
createRoute wrapRoute kmId edit =
    wrapRoute <| CreateRoute kmId edit


toUrl : Route -> List String
toUrl route =
    case route of
        CreateRoute mbSelected mbEdit ->
            case ( mbSelected, mbEdit ) of
                ( Just id, Just edit ) ->
                    let
                        editString =
                            if edit then
                                "true"

                            else
                                "false"
                    in
                    [ moduleRoot
                    , "create"
                    , "?" ++ "selected" ++ "=" ++ id ++ "&" ++ "edit" ++ "=" ++ editString
                    ]

                ( Just id, Nothing ) ->
                    [ moduleRoot
                    , "create"
                    , "?" ++ "selected" ++ "=" ++ id
                    ]

                _ ->
                    [ moduleRoot, "create" ]

        EditorRoute uuid subroute ->
            let
                base =
                    [ moduleRoot, "editor", Uuid.toString uuid ]
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
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        MigrationRoute uuid ->
            [ moduleRoot, "migration", Uuid.toString uuid ]

        PublishRoute uuid ->
            [ moduleRoot, "publish", Uuid.toString uuid ]


isAllowed : Route -> AppState -> Bool
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
