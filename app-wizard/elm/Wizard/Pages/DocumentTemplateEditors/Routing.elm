module Wizard.Pages.DocumentTemplateEditors.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Flip exposing (flip)
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Query as Query
import Url.Parser.Query.Extra as Query
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute
import Wizard.Pages.DocumentTemplateEditors.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "document-template-editors"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (createRoute wrapRoute) (s moduleRoot </> s "create" <?> Query.string "selected" <?> Query.bool "edit")
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "updatedAt,desc")) (PaginationQueryString.parser (s moduleRoot))
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Files) (s moduleRoot </> string)
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Preview) (s moduleRoot </> string </> s "preview")
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Settings) (s moduleRoot </> string </> s "settings")
    ]


createRoute : (Route -> a) -> Maybe String -> Maybe Bool -> a
createRoute wrapRoute documentTemplateId edit =
    wrapRoute <| CreateRoute documentTemplateId edit


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
                    , "?selected=" ++ id ++ "&edit=" ++ editString
                    ]

                ( Just id, Nothing ) ->
                    [ moduleRoot
                    , "create"
                    , "?selected=" ++ id
                    ]

                _ ->
                    [ moduleRoot, "create" ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        EditorRoute templateId subroute ->
            let
                base =
                    [ moduleRoot, templateId ]
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
