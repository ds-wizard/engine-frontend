module Registry.Common.AboutModal exposing
    ( Model
    , Msg(..)
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, code, div, em, h5, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, colspan, href, target)
import Html.Events exposing (onClick)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Requests as Requests
import Registry.Common.View.Page as Page
import Shared.Data.BuildInfo as BuildInfo exposing (BuildInfo)
import Shared.Error.ApiError as ApiError exposing (ApiError)


type alias Model =
    { open : Bool
    , apiBuildInfo : ActionResult BuildInfo
    }


initialModel : Model
initialModel =
    { open = False
    , apiBuildInfo = ActionResult.Unset
    }


setApiBuildInfo : ActionResult BuildInfo -> Model -> Model
setApiBuildInfo apiBuildInfo model =
    { model | apiBuildInfo = apiBuildInfo }


type Msg
    = GetApiBuildInfoComplete (Result ApiError BuildInfo)
    | SetOpen Bool


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetApiBuildInfoComplete result ->
            ( ActionResult.apply setApiBuildInfo (ApiError.toActionResult appState (gettext "Unable to get the build info." appState.locale)) result model
            , Cmd.none
            )

        SetOpen open ->
            if open then
                ( { model
                    | open = True
                    , apiBuildInfo = ActionResult.Loading
                  }
                , Requests.getBuildInfo appState GetApiBuildInfoComplete
                )

            else
                ( { model | open = False }
                , Cmd.none
                )


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "modal modal-cover", classList [ ( "visible", model.open ) ] ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text (gettext "About" appState.locale) ]
                    ]
                , div [ class "modal-body" ]
                    [ Page.actionResultView (viewAboutModalContent appState) model.apiBuildInfo ]
                , div [ class "modal-footer" ]
                    [ button
                        [ onClick (SetOpen False)
                        , class "btn btn-primary"
                        ]
                        [ text (gettext "OK" appState.locale) ]
                    ]
                ]
            ]
        ]


viewAboutModalContent : AppState -> BuildInfo -> Html Msg
viewAboutModalContent appState serverBuildInfo =
    let
        swaggerUrl =
            appState.apiUrl ++ "/swagger-ui/"

        extraServerInfo =
            [ ( gettext "API URL" appState.locale
              , a [ href appState.apiUrl, target "_blank" ]
                    [ text appState.apiUrl ]
              )
            , ( gettext "API Docs" appState.locale
              , a [ href swaggerUrl, target "_blank" ]
                    [ text swaggerUrl ]
              )
            ]
    in
    div []
        [ viewBuildInfo appState (gettext "Client" appState.locale) BuildInfo.client []
        , viewBuildInfo appState (gettext "Server" appState.locale) serverBuildInfo extraServerInfo
        ]


viewBuildInfo : AppState -> String -> BuildInfo -> List ( String, Html msg ) -> Html msg
viewBuildInfo appState name buildInfo extra =
    let
        viewExtraRow ( title, value ) =
            tr []
                [ td [] [ text title ]
                , td [] [ value ]
                ]
    in
    table [ class "table table-borderless table-build-info" ]
        [ thead []
            [ tr []
                [ th [ colspan 2 ] [ text name ] ]
            ]
        , tbody []
            ([ tr []
                [ td [] [ text (gettext "Version" appState.locale) ]
                , td [] [ code [] [ text buildInfo.version ] ]
                ]
             , tr []
                [ td [] [ text (gettext "Built at" appState.locale) ]
                , td [] [ em [] [ text buildInfo.builtAt ] ]
                ]
             ]
                ++ List.map viewExtraRow extra
            )
        ]
