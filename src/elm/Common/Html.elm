module Common.Html exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode
import Msgs exposing (Msg)
import Routing exposing (Route)


linkTo : Route -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
linkTo route attributes children =
    let
        hrefAttributes =
            [ href <| Routing.toUrl route
            , onLinkClick <| Msgs.ChangeLocation <| Routing.toUrl route
            ]
    in
    a (attributes ++ hrefAttributes) children


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
    onWithOptions "click" options (Decode.succeed message)


pageHeader : String -> List (Html Msg) -> Html Msg
pageHeader title actions =
    div [ class "header" ]
        [ h2 [] [ text title ]
        , pageActions actions
        ]


pageActions : List (Html Msg) -> Html Msg
pageActions actions =
    div [ class "actions" ]
        actions


fullPageLoader : Html Msg
fullPageLoader =
    div [ class "full-page-loader" ]
        [ i [ class "fa fa-spinner fa-spin" ] []
        , div [] [ text "Loading..." ]
        ]


defaultFullPageError : String -> Html msg
defaultFullPageError =
    fullPageError "fa-frown-o"


fullPageError : String -> String -> Html msg
fullPageError icon error =
    div [ class "jumbotron full-page-error col-xs-12 col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2 col-lg-6 col-lg-offset-3" ]
        [ h1 [ class "display-3" ] [ i [ class ("fa " ++ icon) ] [] ]
        , p [] [ text error ]
        ]
