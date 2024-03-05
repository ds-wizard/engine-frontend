module Registry2.Pages.Homepage exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, h1, p, span, text)
import Html.Attributes exposing (class, href)
import Registry2.Components.FontAwesome exposing (fas)
import Registry2.Data.AppState exposing (AppState)
import Registry2.Routes as Routes


view : AppState -> Html msg
view appState =
    div [ class "py-5 homepage" ]
        [ div [ class "row" ]
            [ div [ class "col-12 mb-5" ]
                [ h1 [ class "text-center" ]
                    [ text "DSW Registry"
                    ]
                , p [ class "text-center" ]
                    [ text "Customize your experience with prepared content and translations."
                    ]
                ]
            , div [ class "row" ]
                [ viewLink
                    { route = Routes.KnowledgeModels
                    , icon = "fa-sitemap"
                    , title = gettext "Knowledge Models" appState.locale
                    , description = gettext "Choose the appropriate Knowledge Model to shape the structure of your questionnaire." appState.locale
                    }
                , viewLink
                    { route = Routes.DocumentTemplates
                    , icon = "fa-file-code"
                    , title = gettext "Document Templates" appState.locale
                    , description = gettext "Compose documents from questionnaires by selecting templates that handle the transformation of replies." appState.locale
                    }
                , viewLink
                    { route = Routes.Locales
                    , icon = "fa-language"
                    , title = gettext "Locales" appState.locale
                    , description = gettext "Adapt the user interface to different languages for a more inclusive experience." appState.locale
                    }
                ]
            ]
        ]


type alias LinkConfig =
    { route : Routes.Route
    , icon : String
    , title : String
    , description : String
    }


viewLink : LinkConfig -> Html msg
viewLink cfg =
    div [ class "col-12 mb-3 col-lg-4" ]
        [ a
            [ class "shadow rounded bg-white p-4  d-flex flex-column align-items-center text-center homepage-link"
            , href (Routes.toUrl cfg.route)
            ]
            [ fas cfg.icon
            , span [ class "fs-4 my-3" ] [ text cfg.title ]
            , div [ class "text-muted" ] [ text cfg.description ]
            ]
        ]
