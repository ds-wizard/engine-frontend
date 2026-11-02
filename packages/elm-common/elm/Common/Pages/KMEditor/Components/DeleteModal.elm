module Common.Pages.KMEditor.Components.DeleteModal exposing
    ( Model
    , Msg
    , Target
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

{-| Deleting a knowledge model editor, behind a confirmation.

Built on `Common.Components.ConfirmModal.delete`: a failure stays inside the dialog, and
the listing moves focus once the row is gone (`Listing.onAfterDelete`, sent by
the caller's `cmdDeleted`). Dismissing it runs `cmdClosed` with the editor's
uuid, for returning focus to the menu that opened it.

-}

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.EntityCard as EntityCard
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)


type alias Model =
    { kmEditor : Maybe Target
    , deletingKmEditor : ActionResult String
    }


initialModel : Model
initialModel =
    { kmEditor = Nothing
    , deletingKmEditor = ActionResult.Unset
    }


{-| Which editor: its uuid to delete it by, and its name and knowledge model id
to show it by.
-}
type alias Target =
    { uuid : Uuid
    , name : String
    , id : String
    }


type Msg
    = Open Target
    | Delete
    | DeleteComplete (Result ApiError ())
    | Close


open : { a | uuid : Uuid, name : String, id : String } -> Msg
open kmEditor =
    Open { uuid = kmEditor.uuid, name = kmEditor.name, id = kmEditor.id }


type alias UpdateConfig msg =
    { cmdDeleted : Cmd msg
    , cmdClosed : Uuid -> Cmd msg
    , wrapMsg : Msg -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Open target ->
            ( { model | kmEditor = Just target, deletingKmEditor = ActionResult.Unset }, Cmd.none )

        Delete ->
            case model.kmEditor of
                Just target ->
                    ( { model | deletingKmEditor = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <| KnowledgeModelEditorsApi.deleteKnowledgeModelEditor appState target.uuid DeleteComplete
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteComplete result ->
            case result of
                Ok _ ->
                    ( { model | kmEditor = Nothing, deletingKmEditor = ActionResult.Unset }, cfg.cmdDeleted )

                Err error ->
                    ( { model | deletingKmEditor = ApiError.toActionResult appState (gettext "Knowledge model could not be deleted." appState.locale) error }
                    , Cmd.none
                    )

        Close ->
            -- Closing without deleting; `cmdClosed` puts focus back where the
            -- dialog was opened from. After a delete the editor is already gone.
            ( { model | kmEditor = Nothing }
            , Maybe.unwrap Cmd.none (cfg.cmdClosed << .uuid) model.kmEditor
            )


view : AppState -> Model -> Html Msg
view appState model =
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete knowledge model editor" appState.locale
        , open = Maybe.isJust model.kmEditor
        , question = gettext "Are you sure you want to permanently delete this knowledge model editor?" appState.locale
        , entity =
            EntityCard.slot
                (Maybe.map
                    (\kmEditor ->
                        { icon = UserAvatar.item kmEditor.name Nothing
                        , name = kmEditor.name
                        , badges = []
                        , code = Just kmEditor.id
                        , detail = Nothing
                        }
                    )
                    model.kmEditor
                )
        , onConfirm = Delete
        , onClose = Close
        , result = model.deletingKmEditor
        , testId = Just "km-editor-delete"
        }
