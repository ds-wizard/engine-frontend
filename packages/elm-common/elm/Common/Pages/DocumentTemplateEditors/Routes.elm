module Common.Pages.DocumentTemplateEditors.Routes exposing
    ( Route(..)
    , isAllowed
    , parsers
    , toUrl
    )

import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Pages.DocumentTemplateEditors.Editor.Routes as DTEditorRoute exposing (DTEditorRoute)
import Flip exposing (flip)
import Url.Parser exposing ((</>), (<?>), Parser, map, s)
import Url.Parser.Extensions as Parser
import Url.Parser.Query.Extensions as Query
import Uuid exposing (Uuid)


type Route
    = CreateRoute (Maybe Uuid) (Maybe Bool)
    | IndexRoute PaginationQueryString
    | EditorRoute Uuid DTEditorRoute


{-| The editors live under the templates they make.
-}
moduleRoot : List String
moduleRoot =
    [ "document-templates", "editors" ]


root : Parser a a
root =
    s "document-templates" </> s "editors"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (createRoute wrapRoute) (root </> s "create" <?> Query.uuid "selected" <?> Query.bool "edit")
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "updatedAt,desc")) (PaginationQueryString.parser root)
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Files) (root </> Parser.uuid)
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Preview) (root </> Parser.uuid </> s "preview")
    , map (wrapRoute << flip EditorRoute (DTEditorRoute.Settings DTEditorRoute.General)) (root </> Parser.uuid </> s "settings")
    , map (wrapRoute << flip EditorRoute (DTEditorRoute.Settings DTEditorRoute.KnowledgeModels)) (root </> Parser.uuid </> s "settings" </> s "knowledge-models")
    , map (wrapRoute << flip EditorRoute (DTEditorRoute.Settings DTEditorRoute.Formats)) (root </> Parser.uuid </> s "settings" </> s "formats")
    ]


createRoute : (Route -> a) -> Maybe Uuid -> Maybe Bool -> a
createRoute wrapRoute documentTemplateUuid edit =
    wrapRoute <| CreateRoute documentTemplateUuid edit


toUrl : Route -> List String
toUrl route =
    case route of
        CreateRoute mbSelected mbEdit ->
            case ( mbSelected, mbEdit ) of
                ( Just templateUuid, Just edit ) ->
                    let
                        editString =
                            if edit then
                                "true"

                            else
                                "false"
                    in
                    moduleRoot ++ [ "create", "?selected=" ++ Uuid.toString templateUuid ++ "&edit=" ++ editString ]

                ( Just id, Nothing ) ->
                    moduleRoot ++ [ "create", "?selected=" ++ Uuid.toString id ]

                _ ->
                    moduleRoot ++ [ "create" ]

        IndexRoute paginationQueryString ->
            [ "document-templates", "editors" ++ PaginationQueryString.toUrl paginationQueryString ]

        EditorRoute templateUuid subroute ->
            let
                base =
                    moduleRoot ++ [ Uuid.toString templateUuid ]
            in
            case subroute of
                DTEditorRoute.Files ->
                    base

                DTEditorRoute.Preview ->
                    base ++ [ "preview" ]

                DTEditorRoute.Settings DTEditorRoute.General ->
                    base ++ [ "settings" ]

                DTEditorRoute.Settings DTEditorRoute.KnowledgeModels ->
                    base ++ [ "settings", "knowledge-models" ]

                DTEditorRoute.Settings DTEditorRoute.Formats ->
                    base ++ [ "settings", "formats" ]


isAllowed : AppStateLike a -> Route -> Bool
isAllowed appState route =
    case route of
        CreateRoute _ _ ->
            Feature.documentTemplateEditorsCreate appState

        IndexRoute _ ->
            Feature.documentTemplateEditorsView appState

        EditorRoute _ _ ->
            Feature.documentTemplateEditorsEdit appState
