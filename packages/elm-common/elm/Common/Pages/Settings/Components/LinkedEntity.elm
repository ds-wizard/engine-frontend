module Common.Pages.Settings.Components.LinkedEntity exposing
    ( Model
    , Msg
    , decoder
    , initialModel
    , update
    , view
    )

{-| What a configuration's detail links to (`Common.Data.Extensions.LinkedEntity`):
a link to it when the configuration has one, a button that creates it when it
does not. Creating it opens it.
-}

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Context as Context
import Common.Api.Request as Request
import Common.Data.AppState exposing (AppState)
import Common.Data.Extensions exposing (LinkedEntity)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Utils.RequestHelpers as RequestHelpers
import Html exposing (Html)
import Json.Decode as D exposing (Decoder)
import Ui.Button as Button
import Ui.Field as UiField
import Ui.Separator as Separator
import Uuid exposing (Uuid)


type alias Model =
    { creating : ActionResult ()
    }


type Msg
    = Create
    | CreateComplete (Result ApiError Uuid)


initialModel : Model
initialModel =
    { creating = Unset }


{-| The UUID of what the configuration has, read from its detail; nothing when
there is nothing to link to.
-}
decoder : Maybe LinkedEntity -> Decoder (Maybe Uuid)
decoder mbLinkedEntity =
    case mbLinkedEntity of
        Just linkedEntity ->
            D.oneOf
                [ D.field linkedEntity.field (D.nullable Uuid.decoder)
                , D.succeed Nothing
                ]

        Nothing ->
            D.succeed Nothing


{-| `apiPath` is the configuration's address in the API.
-}
update : UpdateConfig Msg msg -> AppState -> { linkedEntity : LinkedEntity, apiPath : String } -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState { linkedEntity, apiPath } msg model =
    case msg of
        Create ->
            ( { model | creating = Loading }
            , Request.postEmptyBody (Context.toServerInfo appState)
                (apiPath ++ linkedEntity.createPath)
                (D.field "uuid" Uuid.decoder)
                (cfg.wrapMsg << CreateComplete)
            )

        CreateComplete result ->
            case result of
                Ok uuid ->
                    ( { model | creating = Success () }
                    , Navigation.pushUrl appState.key (linkedEntity.url uuid)
                    )

                Err error ->
                    ( { model | creating = ApiError.toActionResult appState linkedEntity.createError error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


{-| After a separator, as the last thing in the configuration's form.
-}
view : LinkedEntity -> Maybe Uuid -> Model -> List (Html Msg)
view linkedEntity mbUuid model =
    let
        button =
            case mbUuid of
                Just uuid ->
                    Button.link linkedEntity.viewLabel (linkedEntity.url uuid)

                Nothing ->
                    Button.secondary linkedEntity.createLabel Create
                        |> Button.withLoading (ActionResult.isLoading model.creating)

        error =
            case model.creating of
                Error message ->
                    Just message

                _ ->
                    Nothing
    in
    [ Separator.line
    , UiField.readOnlyHtml
        { id = "linked-" ++ linkedEntity.field
        , label = linkedEntity.label
        , value = [ Button.toHtml button ]
        }
        |> UiField.withHint linkedEntity.hint
        |> UiField.withError error
        |> UiField.toHtml
    ]
