module Wizard.DocumentTemplateEditors.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Shared.Utils exposing (flip)
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Query as Query
import Url.Parser.Query.Extra as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "documentTemplateEditors" appState
    in
    [ map (createRoute wrapRoute) (s moduleRoot </> s "create" <?> Query.string "selected" <?> Query.bool "edit")
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "updatedAt,desc")) (PaginationQueryString.parser (s moduleRoot))
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Template) (s moduleRoot </> string)
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Files) (s moduleRoot </> string </> s "files")
    , map (wrapRoute << flip EditorRoute DTEditorRoute.Preview) (s moduleRoot </> string </> s "preview")
    ]


createRoute : (Route -> a) -> Maybe String -> Maybe Bool -> a
createRoute wrapRoute documentTemplateId edit =
    wrapRoute <| CreateRoute documentTemplateId edit


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "documentTemplateEditors" appState
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
                DTEditorRoute.Template ->
                    base

                DTEditorRoute.Files ->
                    base ++ [ "files" ]

                DTEditorRoute.Preview ->
                    base ++ [ "preview" ]


isAllowed : AppState -> Bool
isAllowed appState =
    Feature.templatesView appState
