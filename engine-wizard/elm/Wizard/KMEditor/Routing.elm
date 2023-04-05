module Wizard.KMEditor.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Shared.Utils exposing (flip)
import Url.Parser exposing ((</>), (<?>), Parser, map, s)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Url.Parser.Query.Extra as Query
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.KMEditor.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "kmEditor" appState

        editorWithEntityRoute editorUuid entityUuid =
            wrapRoute <| EditorRoute editorUuid <| KMEditorRoute.Edit (Just entityUuid)
    in
    [ map (createRoute wrapRoute) (s moduleRoot </> s (lr "kmEditor.create" appState) <?> Query.string (lr "kmEditor.create.selected" appState) <?> Query.bool (lr "kmEditor.create.edit" appState))
    , map (wrapRoute << flip EditorRoute (KMEditorRoute.Edit Nothing)) (s moduleRoot </> s (lr "kmEditor.editor" appState) </> uuid)
    , map editorWithEntityRoute (s moduleRoot </> s (lr "kmEditor.editor" appState) </> uuid </> s "edit" </> uuid)
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Phases) (s moduleRoot </> s (lr "kmEditor.editor" appState) </> uuid </> s "phases")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.QuestionTags) (s moduleRoot </> s (lr "kmEditor.editor" appState) </> uuid </> s "question-tags")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Preview) (s moduleRoot </> s (lr "kmEditor.editor" appState) </> uuid </> s "preview")
    , map (wrapRoute << flip EditorRoute KMEditorRoute.Settings) (s moduleRoot </> s (lr "kmEditor.editor" appState) </> uuid </> s "settings")
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "updatedAt,desc")) (PaginationQueryString.parser (s moduleRoot))
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "kmEditor.migration" appState) </> uuid)
    , map (wrapRoute << PublishRoute) (s moduleRoot </> s (lr "kmEditor.publish" appState) </> uuid)
    ]


createRoute : (Route -> a) -> Maybe String -> Maybe Bool -> a
createRoute wrapRoute kmId edit =
    wrapRoute <| CreateRoute kmId edit


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "kmEditor" appState
    in
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
                    , lr "kmEditor.create" appState
                    , "?" ++ lr "kmEditor.create.selected" appState ++ "=" ++ id ++ "&" ++ lr "kmEditor.create.edit" appState ++ "=" ++ editString
                    ]

                ( Just id, Nothing ) ->
                    [ moduleRoot
                    , lr "kmEditor.create" appState
                    , "?" ++ lr "kmEditor.create.selected" appState ++ "=" ++ id
                    ]

                _ ->
                    [ moduleRoot, lr "kmEditor.create" appState ]

        EditorRoute uuid subroute ->
            let
                base =
                    [ moduleRoot, lr "kmEditor.editor" appState, Uuid.toString uuid ]
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
            [ moduleRoot, lr "kmEditor.migration" appState, Uuid.toString uuid ]

        PublishRoute uuid ->
            [ moduleRoot, lr "kmEditor.publish" appState, Uuid.toString uuid ]


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
