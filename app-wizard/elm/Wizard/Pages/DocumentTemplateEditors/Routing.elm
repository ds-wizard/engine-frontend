module Wizard.Pages.DocumentTemplateEditors.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Flip exposing (flip)
import Url.Parser exposing ((</>), (<?>), Parser, map, s)
import Url.Parser.Extensions as Parser
import Url.Parser.Query.Extensions as Query
import Uuid exposing (Uuid)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute
import Wizard.Pages.DocumentTemplateEditors.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "document-template-editors"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (createRoute wrapRoute) (s moduleRoot </> s "create" <?> Query.uuid "selected" <?> Query.bool "edit")
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "updatedAt,desc")) (PaginationQueryString.parser (s moduleRoot))
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Files) (s moduleRoot </> Parser.uuid)
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Preview) (s moduleRoot </> Parser.uuid </> s "preview")
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Settings) (s moduleRoot </> Parser.uuid </> s "settings")
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
                    [ moduleRoot
                    , "create"
                    , "?selected=" ++ Uuid.toString templateUuid ++ "&edit=" ++ editString
                    ]

                ( Just id, Nothing ) ->
                    [ moduleRoot
                    , "create"
                    , "?selected=" ++ Uuid.toString id
                    ]

                _ ->
                    [ moduleRoot, "create" ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        EditorRoute templateUuid subroute ->
            let
                base =
                    [ moduleRoot, Uuid.toString templateUuid ]
            in
            case subroute of
                DTEditorRoute.Files ->
                    base

                DTEditorRoute.Preview ->
                    base ++ [ "preview" ]

                DTEditorRoute.Settings ->
                    base ++ [ "settings" ]


isAllowed : AppState -> Bool
isAllowed appState =
    Feature.documentTemplatesView appState
