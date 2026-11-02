module Common.Components.Questionnaire.LocalStorage exposing
    ( Stored(..)
    , decode
    , getItems
    , storeCollapsedPaths
    , storeNamedOnly
    , storeRightPanel
    , storeViewResolved
    , storeViewSettings
    )

{-| What the questionnaire remembers between visits, in local storage:

  - `project-view-settings` — the View menu, for every project;
  - `project-{uuid}-collapsed-paths` — what is collapsed in this project;
  - `project-{uuid}-right-panel` — the panel left open;
  - `project-{uuid}-view-resolved` — whether resolved comment threads show;
  - `project-{uuid}-named-only` — whether the version history shows named
    versions only.

A questionnaire that is not a project's — a knowledge model's preview, whose
uuid is nil — keeps the View menu and nothing per project: every preview would
otherwise share one set of folded paths and one open panel.

-}

import Common.Components.Questionnaire.RightPanel as RightPanel exposing (RightPanel)
import Common.Components.Questionnaire.ViewSettings as ViewSettings exposing (ViewSettings)
import Common.Ports.LocalStorage as LocalStorage
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Set exposing (Set)
import Uuid exposing (Uuid)


type Stored
    = ViewSettingsStored ViewSettings
    | CollapsedPathsStored (Set String)
    | RightPanelStored RightPanel
    | ViewResolvedStored Bool
    | NamedOnlyStored Bool


viewSettingsKey : String
viewSettingsKey =
    "project-view-settings"


projectKey : String -> Uuid -> String
projectKey name uuid =
    "project-" ++ Uuid.toString uuid ++ "-" ++ name


{-| Ask for everything stored for the project. `withRightPanel` is false when a
deep link decides the panel (the comments of a question), so a stored one arriving
later does not replace it.
-}
getItems : Uuid -> Bool -> Cmd msg
getItems uuid withRightPanel =
    if isProject uuid then
        Cmd.batch
            (LocalStorage.getItem viewSettingsKey
                :: LocalStorage.getItem (projectKey "collapsed-paths" uuid)
                :: LocalStorage.getItem (projectKey "view-resolved" uuid)
                :: LocalStorage.getItem (projectKey "named-only" uuid)
                :: (if withRightPanel then
                        [ LocalStorage.getItem (projectKey "right-panel" uuid) ]

                    else
                        []
                   )
            )

    else
        LocalStorage.getItem viewSettingsKey


isProject : Uuid -> Bool
isProject uuid =
    uuid /= Uuid.nil


{-| Set a project's item, when the questionnaire is a project's.
-}
setProjectItem : String -> Uuid -> E.Value -> Cmd msg
setProjectItem name uuid value =
    if isProject uuid then
        LocalStorage.setItem (projectKey name uuid) value

    else
        Cmd.none


storeViewSettings : ViewSettings -> Cmd msg
storeViewSettings viewSettings =
    LocalStorage.setItem viewSettingsKey (ViewSettings.encode viewSettings)


storeCollapsedPaths : Uuid -> Set String -> Cmd msg
storeCollapsedPaths uuid collapsedPaths =
    if Set.isEmpty collapsedPaths && isProject uuid then
        LocalStorage.removeItem (projectKey "collapsed-paths" uuid)

    else
        setProjectItem "collapsed-paths" uuid (E.set E.string collapsedPaths)


storeRightPanel : Uuid -> RightPanel -> Cmd msg
storeRightPanel uuid rightPanel =
    setProjectItem "right-panel" uuid (RightPanel.encode rightPanel)


storeViewResolved : Uuid -> Bool -> Cmd msg
storeViewResolved uuid viewResolved =
    setProjectItem "view-resolved" uuid (E.bool viewResolved)


storeNamedOnly : Uuid -> Bool -> Cmd msg
storeNamedOnly uuid namedOnly =
    setProjectItem "named-only" uuid (E.bool namedOnly)


{-| An item the local storage port answered with, if it is one of this
project's. Anything else — another page's item, a value in a shape this release
does not read — is `Nothing`.
-}
decode : Uuid -> E.Value -> Maybe Stored
decode uuid json =
    let
        value : Decoder a -> (a -> Stored) -> Maybe Stored
        value decoder toStored =
            LocalStorage.decodeItemValue decoder json
                |> Result.toMaybe
                |> Maybe.map toStored
    in
    case D.decodeValue (D.field "key" D.string) json of
        Ok key ->
            if key == viewSettingsKey then
                value ViewSettings.decoder ViewSettingsStored

            else if key == projectKey "collapsed-paths" uuid then
                value (D.map Set.fromList (D.list D.string)) CollapsedPathsStored

            else if key == projectKey "right-panel" uuid then
                value RightPanel.decoder RightPanelStored

            else if key == projectKey "view-resolved" uuid then
                value D.bool ViewResolvedStored

            else if key == projectKey "named-only" uuid then
                value D.bool NamedOnlyStored

            else
                Nothing

        Err _ ->
            Nothing
