module Registry.Pages.LocalesDetail exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, a, br, code, div, h5, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, href, target, title)
import Html.Events exposing (onClick)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Entities.LocaleDetail exposing (LocaleDetail)
import Registry.Common.Entities.OrganizationInfo exposing (OrganizationInfo)
import Registry.Common.Requests as Requests
import Registry.Common.View.ItemIcon as ItemIcon
import Registry.Common.View.Page as Page
import Registry.Routing as Routing
import Shared.Copy as Copy
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode)
import Shared.Markdown as Markdown
import Version


init : AppState -> String -> ( Model, Cmd Msg )
init appState localeId =
    ( { locale = Loading
      , copied = False
      }
    , Requests.getLocale appState localeId GetLocaleComplete
    )



-- MODEL


type alias Model =
    { locale : ActionResult LocaleDetail
    , copied : Bool
    }


setLocale : ActionResult LocaleDetail -> Model -> Model
setLocale locale model =
    { model | locale = locale }



-- UPDATE


type Msg
    = GetLocaleComplete (Result ApiError LocaleDetail)
    | CopyLocaleId String


update : Msg -> AppState -> Model -> ( Model, Cmd msg )
update msg appState model =
    case msg of
        GetLocaleComplete result ->
            ( ActionResult.apply setLocale (ApiError.toActionResult appState (gettext "Unable to get the locale." appState.locale)) result model
            , Cmd.none
            )

        CopyLocaleId localeId ->
            ( { model | copied = True }, Copy.copyToClipboard localeId )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewDetail appState model) model.locale


viewDetail : AppState -> Model -> LocaleDetail -> Html Msg
viewDetail appState model locale =
    let
        viewLocaleIdCopied =
            if model.copied then
                span [ class "ms-2 text-muted" ] [ text (gettext "Copied!" appState.locale) ]

            else
                emptyNode

        viewLocaleId =
            [ h5 [] [ text (gettext "Locale ID" appState.locale) ]
            , p []
                [ code
                    [ onClick (CopyLocaleId locale.id)
                    , title (gettext "Click to copy locale ID" appState.locale)
                    , class "entity-id"
                    ]
                    [ text locale.id ]
                , viewLocaleIdCopied
                ]
            ]

        viewLanguageCode =
            [ h5 [] [ text (gettext "Language Code" appState.locale) ]
            , p []
                [ code [] [ text locale.code ]
                ]
            ]

        viewPublishedBy =
            [ h5 [] [ text (gettext "Published by" appState.locale) ]
            , viewOrganization locale.organization
            ]

        viewLicense =
            [ h5 [] [ text (gettext "License" appState.locale) ]
            , p []
                [ a [ href <| "https://spdx.org/licenses/" ++ locale.license ++ ".html", target "_blank" ]
                    [ text locale.license ]
                ]
            ]

        viewCurrentVersion =
            [ h5 [] [ text (gettext "Version" appState.locale) ]
            , p [] [ text <| Version.toString locale.version ]
            ]

        otherVersions =
            locale.versions
                |> List.filter ((/=) locale.version)
                |> List.sortWith Version.compare
                |> List.reverse

        viewOtherVersions =
            case otherVersions of
                [] ->
                    []

                versions ->
                    [ h5 [] [ text (gettext "Other versions" appState.locale) ]
                    , ul []
                        (List.map viewVersion versions)
                    ]

        viewVersion version =
            li []
                [ a [ href <| Routing.toString <| Routing.DocumentTemplateDetail (locale.organization.organizationId ++ ":" ++ locale.localeId ++ ":" ++ Version.toString version) ]
                    [ text <| Version.toString version ]
                ]

        viewSupportedWizardVersion =
            [ h5 [] [ text (gettext "Metamodel version" appState.locale) ]
            , p [] [ text <| Version.toString locale.recommendedAppVersion ]
            ]
    in
    div [ class "Detail" ]
        [ div [ class "row" ]
            [ div [ class "col-12 col-md-8" ]
                [ Markdown.toHtml [] locale.readme ]
            , div [ class "Detail__Panel col-12 col-md-4" ]
                (viewLocaleId
                    ++ viewLanguageCode
                    ++ viewPublishedBy
                    ++ viewLicense
                    ++ viewSupportedWizardVersion
                    ++ viewCurrentVersion
                    ++ viewOtherVersions
                )
            ]
        ]


viewOrganization : OrganizationInfo -> Html msg
viewOrganization organization =
    div [ class "organization" ]
        [ ItemIcon.view { text = organization.name, image = organization.logo }
        , div [ class "content" ]
            [ strong [] [ text organization.name ]
            , br [] []
            , text organization.organizationId
            ]
        ]
