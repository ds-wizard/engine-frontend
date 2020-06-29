module Registry.Pages.TemplateDetail exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, a, br, code, div, h5, li, p, strong, text, ul)
import Html.Attributes exposing (class, href, target)
import Markdown
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Entities.OrganizationInfo exposing (OrganizationInfo)
import Registry.Common.Entities.TemplateDetail exposing (TemplateDetail)
import Registry.Common.Requests as Requests
import Registry.Common.View.ItemIcon as ItemIcon
import Registry.Common.View.Page as Page
import Registry.Routing as Routing
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lx)
import Version


l_ : String -> AppState -> String
l_ =
    l "Registry.Pages.TemplateDetail"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Registry.Pages.TemplateDetail"


init : AppState -> String -> ( Model, Cmd Msg )
init appState templateId =
    ( { template = Loading }
    , Requests.getTemplate appState templateId GetTemplateCompleted
    )



-- MODEL


type alias Model =
    { template : ActionResult TemplateDetail }


setTemplate : ActionResult TemplateDetail -> Model -> Model
setTemplate template model =
    { model | template = template }



-- UPDATE


type Msg
    = GetTemplateCompleted (Result ApiError TemplateDetail)


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        GetTemplateCompleted result ->
            ActionResult.apply setTemplate (ApiError.toActionResult (l_ "update.getError" appState)) result



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewDetail appState) model.template


viewDetail : AppState -> TemplateDetail -> Html Msg
viewDetail appState template =
    let
        viewKmId =
            [ h5 [] [ lx_ "view.templateId" appState ]
            , p []
                [ code [] [ text template.id ] ]
            ]

        viewPublishedBy =
            [ h5 [] [ lx_ "view.publishedBy" appState ]
            , viewOrganization template.organization
            ]

        viewLicense =
            [ h5 [] [ lx_ "view.license" appState ]
            , p []
                [ a [ href <| "https://spdx.org/licenses/" ++ template.license ++ ".html", target "_blank" ]
                    [ text template.license ]
                ]
            ]

        viewCurrentVersion =
            [ h5 [] [ lx_ "view.version" appState ]
            , p [] [ text <| Version.toString template.version ]
            ]

        otherVersions =
            template.versions
                |> List.filter ((/=) template.version)
                |> List.sortWith Version.compare
                |> List.reverse

        viewOtherVersions =
            case otherVersions of
                [] ->
                    []

                versions ->
                    [ h5 [] [ lx_ "view.otherVersions" appState ]
                    , ul []
                        (List.map viewVersion versions)
                    ]

        viewVersion version =
            li []
                [ a [ href <| Routing.toString <| Routing.KMDetail (template.organization.organizationId ++ ":" ++ template.templateId ++ ":" ++ Version.toString version) ]
                    [ text <| Version.toString version ]
                ]

        viewSupportedMetamodel =
            [ h5 [] [ lx_ "view.metamodelVersion" appState ]
            , p [] [ text <| String.fromInt template.metamodelVersion ]
            ]
    in
    div [ class "Detail" ]
        [ div [ class "row" ]
            [ div [ class "col-12 col-md-8" ]
                [ Markdown.toHtml [] template.readme ]
            , div [ class "Detail__Panel col-12 col-md-4" ]
                (viewKmId
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
