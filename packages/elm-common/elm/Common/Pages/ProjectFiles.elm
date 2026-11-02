module Common.Pages.ProjectFiles exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| Project Files: the feature router.
-}

import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.ProjectFiles.Index as Index
import Common.Pages.ProjectFiles.Routes as ProjectFilesRoutes
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
        { index = Index.initialModel }


init : AppState -> Model -> ProjectFilesRoutes.Route -> ( Model, Cmd Msg )
init =
    \_ model route ->
        case route of
            ProjectFilesRoutes.IndexRoute pqs ->
                let
                    ( index, cmd ) =
                        Index.init pqs
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


subscriptions : AppState -> ProjectFilesRoutes.Route -> Model -> Sub Msg
subscriptions =
    \_ _ model ->
        Sub.map IndexMsg (Index.subscriptions model.index)


view : AppState -> ProjectFilesRoutes.Route -> Model -> Html Msg
view =
    \appState _ model ->
        Html.map IndexMsg (Index.view appState model.index)


isGuarded : AppState -> ProjectFilesRoutes.Route -> Model -> Maybe String
isGuarded =
    \_ _ _ ->
        Nothing
