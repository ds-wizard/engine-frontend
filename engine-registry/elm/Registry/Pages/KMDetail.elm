module Registry.Pages.KMDetail exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, a, br, code, div, h5, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, href, target, title)
import Html.Events exposing (onClick)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Entities.OrganizationInfo exposing (OrganizationInfo)
import Registry.Common.Entities.PackageDetail exposing (PackageDetail)
import Registry.Common.Requests as Requests
import Registry.Common.View.ItemIcon as ItemIcon
import Registry.Common.View.Page as Page
import Registry.Routing as Routing
import Shared.Copy as Copy
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lx)
import Shared.Markdown as Markdown
import Version


l_ : String -> AppState -> String
l_ =
    l "Registry.Pages.KMDetail"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Registry.Pages.KMDetail"


init : AppState -> String -> ( Model, Cmd Msg )
init appState packageId =
    ( { package = Loading
      , copied = False
      }
    , Requests.getPackage appState packageId GetPackageCompleted
    )



-- MODEL


type alias Model =
    { package : ActionResult PackageDetail
    , copied : Bool
    }


setPackage : ActionResult PackageDetail -> Model -> Model
setPackage package model =
    { model | package = package }



-- UPDATE


type Msg
    = GetPackageCompleted (Result ApiError PackageDetail)
    | CopyKmId String


update : Msg -> AppState -> Model -> ( Model, Cmd msg )
update msg appState model =
    case msg of
        GetPackageCompleted result ->
            ( ActionResult.apply setPackage (ApiError.toActionResult appState (l_ "update.getError" appState)) result model
            , Cmd.none
            )

        CopyKmId kmId ->
            ( { model | copied = True }, Copy.copyToClipboard kmId )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewDetail appState model) model.package


viewDetail : AppState -> Model -> PackageDetail -> Html Msg
viewDetail appState model package =
    let
        viewKmIdCopied =
            if model.copied then
                span [ class "ms-2 text-muted" ] [ lx_ "view.kmId.copied" appState ]

            else
                emptyNode

        viewKmId =
            [ h5 [] [ lx_ "view.kmId" appState ]
            , p []
                [ code
                    [ onClick (CopyKmId package.id)
                    , title (l_ "view.kmId.copy" appState)
                    , class "entity-id"
                    ]
                    [ text package.id ]
                , viewKmIdCopied
                ]
            ]

        viewPublishedBy =
            [ h5 [] [ lx_ "view.publishedBy" appState ]
            , viewOrganization package.organization
            ]

        viewLicense =
            [ h5 [] [ lx_ "view.license" appState ]
            , p []
                [ a [ href <| "https://spdx.org/licenses/" ++ package.license ++ ".html", target "_blank" ]
                    [ text package.license ]
                ]
            ]

        viewCurrentVersion =
            [ h5 [] [ lx_ "view.version" appState ]
            , p [] [ text <| Version.toString package.version ]
            ]

        otherVersions =
            package.versions
                |> List.filter ((/=) package.version)
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
                [ a [ href <| Routing.toString <| Routing.KMDetail (package.organization.organizationId ++ ":" ++ package.kmId ++ ":" ++ Version.toString version) ]
                    [ text <| Version.toString version ]
                ]

        viewSupportedMetamodel =
            [ h5 [] [ lx_ "view.metamodelVersion" appState ]
            , p [] [ text <| String.fromInt package.metamodelVersion ]
            ]

        viewParentKnowledgeModel =
            case package.forkOfPackageId of
                Just parentPackageId ->
                    [ h5 [] [ lx_ "view.forkOf" appState ]
                    , p []
                        [ a [ href <| Routing.toString <| Routing.KMDetail parentPackageId ]
                            [ text parentPackageId
                            ]
                        ]
                    ]

                Nothing ->
                    []
    in
    div [ class "Detail" ]
        [ div [ class "row" ]
            [ div [ class "col-12 col-md-8" ]
                [ Markdown.toHtml [] package.readme ]
            , div [ class "Detail__Panel col-12 col-md-4" ]
                (viewKmId
                    ++ viewPublishedBy
                    ++ viewLicense
                    ++ viewCurrentVersion
                    ++ viewOtherVersions
                    ++ viewSupportedMetamodel
                    ++ viewParentKnowledgeModel
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
