module Registry.Pages.DocumentTemplateDetail exposing
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
import Registry.Common.Entities.DocumentTemplateDetail exposing (DocumentTemplateDetail)
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
init appState templateId =
    ( { documentTemplate = Loading
      , copied = False
      }
    , Requests.getDocumentTemplate appState templateId GetDocumentTemplateCompleted
    )



-- MODEL


type alias Model =
    { documentTemplate : ActionResult DocumentTemplateDetail
    , copied : Bool
    }


setDocumentTemplate : ActionResult DocumentTemplateDetail -> Model -> Model
setDocumentTemplate template model =
    { model | documentTemplate = template }



-- UPDATE


type Msg
    = GetDocumentTemplateCompleted (Result ApiError DocumentTemplateDetail)
    | CopyDocumentTemplateId String


update : Msg -> AppState -> Model -> ( Model, Cmd msg )
update msg appState model =
    case msg of
        GetDocumentTemplateCompleted result ->
            ( ActionResult.apply setDocumentTemplate (ApiError.toActionResult appState (gettext "Unable to get the template." appState.locale)) result model
            , Cmd.none
            )

        CopyDocumentTemplateId templateId ->
            ( { model | copied = True }, Copy.copyToClipboard templateId )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewDetail appState model) model.documentTemplate


viewDetail : AppState -> Model -> DocumentTemplateDetail -> Html Msg
viewDetail appState model documentTemplate =
    let
        viewTemplateIdCopied =
            if model.copied then
                span [ class "ms-2 text-muted" ] [ text (gettext "Copied!" appState.locale) ]

            else
                emptyNode

        viewTemplateId =
            [ h5 [] [ text (gettext "Template ID" appState.locale) ]
            , p []
                [ code
                    [ onClick (CopyDocumentTemplateId documentTemplate.id)
                    , title (gettext "Click to copy Template ID" appState.locale)
                    , class "entity-id"
                    ]
                    [ text documentTemplate.id ]
                , viewTemplateIdCopied
                ]
            ]

        viewPublishedBy =
            [ h5 [] [ text (gettext "Published by" appState.locale) ]
            , viewOrganization documentTemplate.organization
            ]

        viewLicense =
            [ h5 [] [ text (gettext "License" appState.locale) ]
            , p []
                [ a [ href <| "https://spdx.org/licenses/" ++ documentTemplate.license ++ ".html", target "_blank" ]
                    [ text documentTemplate.license ]
                ]
            ]

        viewCurrentVersion =
            [ h5 [] [ text (gettext "Version" appState.locale) ]
            , p [] [ text <| Version.toString documentTemplate.version ]
            ]

        otherVersions =
            documentTemplate.versions
                |> List.filter ((/=) documentTemplate.version)
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
                [ a [ href <| Routing.toString <| Routing.DocumentTemplateDetail (documentTemplate.organization.organizationId ++ ":" ++ documentTemplate.templateId ++ ":" ++ Version.toString version) ]
                    [ text <| Version.toString version ]
                ]

        viewSupportedMetamodel =
            [ h5 [] [ text (gettext "Metamodel version" appState.locale) ]
            , p [] [ text <| String.fromInt documentTemplate.metamodelVersion ]
            ]
    in
    div [ class "Detail" ]
        [ div [ class "row" ]
            [ div [ class "col-12 col-md-8" ]
                [ Markdown.toHtml [] documentTemplate.readme ]
            , div [ class "Detail__Panel col-12 col-md-4" ]
                (viewTemplateId
                    ++ viewPublishedBy
                    ++ viewLicense
                    ++ viewCurrentVersion
                    ++ viewOtherVersions
                    ++ viewSupportedMetamodel
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
