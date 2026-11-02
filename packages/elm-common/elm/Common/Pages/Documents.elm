module Common.Pages.Documents exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| Documents: the feature router.
-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Documents.Index as Index
import Common.Pages.Documents.Routes as DocumentsRoutes
import Html exposing (Html)
import Random


type alias Model =
    { index : Index.Model
    }


type Msg
    = IndexMsg Index.Msg


initialModel : AppState -> Model
initialModel =
    \_ ->
        { index = Index.initialModel Nothing PaginationQueryString.empty }


init : AppState -> Model -> DocumentsRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        DocumentsRoutes.IndexRoute projectUuid pqs ->
            let
                ( index, cmd ) =
                    Index.init appState projectUuid pqs
            in
            ( { model | index = index }, Cmd.map IndexMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        IndexMsg indexMsg ->
            let
                ( index, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState indexMsg model.index
            in
            ( appState.seed, { model | index = index }, cmd )


subscriptions : AppState -> DocumentsRoutes.Route -> Model -> Sub Msg
subscriptions =
    \_ _ model ->
        Sub.map IndexMsg (Index.subscriptions model.index)


view : AppState -> DocumentsRoutes.Route -> Model -> Html Msg
view =
    \appState _ model ->
        Html.map IndexMsg (Index.view appState model.index)


isGuarded : AppState -> DocumentsRoutes.Route -> Model -> Maybe String
isGuarded =
    \_ _ _ ->
        Nothing
