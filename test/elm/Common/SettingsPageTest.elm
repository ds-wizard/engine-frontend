module Common.SettingsPageTest exposing (settingsPageTest)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError
import Common.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.Settings exposing (Settings)
import Common.Api.Params as Params exposing (RequestParams)
import Common.Components.SettingsPage as SettingsPage
import Common.Data.Session as Session exposing (Session)
import Common.Utils.Form.FormError exposing (FormError)
import Expect
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Gettext
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)


type alias Config =
    { name : String
    , links : List String
    }


type alias TestForm =
    { name : String
    , links : List String
    }


validation : Validation FormError TestForm
validation =
    V.succeed TestForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "links" (V.list (V.field "url" V.string)))


initForm : Config -> Form FormError TestForm
initForm config =
    Form.initial
        [ ( "name", Field.string config.name )
        , ( "links", Field.list (List.map (\url -> Field.group [ ( "url", Field.string url ) ]) config.links) )
        ]
        validation


loadedValue : Config
loadedValue =
    { name = "Organization", links = [ "a", "b" ] }


{-| A section with nothing about overriding: a plain form.
-}
loadedConfig : Settings Config
loadedConfig =
    { value = loadedValue, overrideAllowed = Nothing, overridden = Nothing }


{-| The defaults of a section that may be overridden.
-}
defaults : Settings Config
defaults =
    { value = loadedValue, overrideAllowed = Just True, overridden = Nothing }


{-| A section read where it may not be overridden.
-}
locked : Settings Config
locked =
    { value = loadedValue, overrideAllowed = Just False, overridden = Just False }


updateConfig : SettingsPage.UpdateConfig Config TestForm (SettingsPage.Msg Config)
updateConfig =
    { wrapMsg = identity
    , logoutMsg = SettingsPage.formMsg Form.NoOp
    , section = { name = "test", decoder = D.fail "not read", encode = always E.null }
    , initForm = initForm
    , formToValue = \form _ -> { name = form.name, links = form.links }
    , formValidation = validation
    }


appState :
    { locale : Gettext.Locale
    , apiUrl : String
    , session : Session
    , config : BootstrapConfig
    , urlCheckerUrl : Maybe String
    , requestParams : RequestParams
    }
appState =
    { locale = Gettext.defaultLocale
    , apiUrl = ""
    , session = Session.init ""
    , config = BootstrapConfig.default
    , urlCheckerUrl = Nothing
    , requestParams = Params.none
    }


run : List (SettingsPage.Msg Config) -> SettingsPage.Model Config TestForm -> SettingsPage.Model Config TestForm
run msgs model =
    List.foldl (\msg acc -> Tuple.first (SettingsPage.update appState updateConfig msg acc)) model msgs


loaded : SettingsPage.Model Config TestForm
loaded =
    loadedWith loadedConfig


loadedWith : Settings Config -> SettingsPage.Model Config TestForm
loadedWith config =
    SettingsPage.initialModel (Form.initial [] validation)
        |> run [ SettingsPage.getConfigCompleted (Ok config) ]


settingsPageTest : Test
settingsPageTest =
    describe "Common.Components.SettingsPage"
        [ describe "isDirty"
            [ test "is clean after loading" <|
                \_ ->
                    SettingsPage.isDirty loaded
                        |> Expect.equal False
            , test "is dirty after a field changes" <|
                \_ ->
                    run [ SettingsPage.formMsg (Form.Input "name" Form.Text (Field.String "Other")) ] loaded
                        |> SettingsPage.isDirty
                        |> Expect.equal True
            , test "is dirty after removing a list item, which changes no field" <|
                \_ ->
                    run [ SettingsPage.formMsg (Form.RemoveItem "links" 1) ] loaded
                        |> SettingsPage.isDirty
                        |> Expect.equal True
            , test "is dirty after moving a list item" <|
                \_ ->
                    run [ SettingsPage.moveItemDown "links" [ "url" ] 0 ] loaded
                        |> SettingsPage.isDirty
                        |> Expect.equal True
            , test "ignores a move past the end of the list" <|
                \_ ->
                    run [ SettingsPage.moveItemDown "links" [ "url" ] 1 ] loaded
                        |> SettingsPage.isDirty
                        |> Expect.equal False
            , test "is clean again after discarding" <|
                \_ ->
                    run
                        [ SettingsPage.formMsg (Form.Input "name" Form.Text (Field.String "Other"))
                        , SettingsPage.formMsg (Form.RemoveItem "links" 1)
                        ]
                        loaded
                        |> SettingsPage.update appState updateConfig SettingsPage.discard
                        |> Tuple.first
                        |> SettingsPage.isDirty
                        |> Expect.equal False
            ]
        , describe "isUnsavedBarVisible"
            [ test "is hidden when nothing changed" <|
                \_ ->
                    SettingsPage.isUnsavedBarVisible loaded
                        |> Expect.equal False
            , test "stays up while saving" <|
                \_ ->
                    SettingsPage.isUnsavedBarVisible { loaded | savingConfig = Loading }
                        |> Expect.equal True
            , test "goes after a successful save, before the reload" <|
                \_ ->
                    run [ SettingsPage.formMsg (Form.Input "name" Form.Text (Field.String "Other")) ]
                        { loaded | savingConfig = Success () }
                        |> (\model -> { model | savingConfig = Success () })
                        |> SettingsPage.isUnsavedBarVisible
                        |> Expect.equal False
            , test "goes when a failed save is followed by a discard" <|
                \_ ->
                    run
                        [ SettingsPage.formMsg (Form.Input "name" Form.Text (Field.String "Other"))
                        , SettingsPage.discard
                        ]
                        { loaded | savingConfig = Error "failed" }
                        |> SettingsPage.isUnsavedBarVisible
                        |> Expect.equal False
            ]
        , describe "submit"
            [ test "a valid submit starts saving" <|
                \_ ->
                    run [ SettingsPage.formMsg (Form.Input "name" Form.Text (Field.String "Other")), SettingsPage.formMsg Form.Submit ] loaded
                        |> .savingConfig
                        |> Expect.equal Loading
            , test "an invalid submit does not save" <|
                \_ ->
                    run [ SettingsPage.formMsg (Form.Input "name" Form.Text (Field.String "")), SettingsPage.formMsg Form.Submit ] loaded
                        |> .savingConfig
                        |> Expect.equal Unset
            , test "a submit before the configuration has loaded does not save" <|
                \_ ->
                    run [ SettingsPage.formMsg Form.Submit ] (SettingsPage.initialModel (initForm loadedValue))
                        |> .savingConfig
                        |> Expect.equal Unset
            , test "a submit where the section may not be overridden does not save" <|
                \_ ->
                    run [ SettingsPage.formMsg (Form.Input "name" Form.Text (Field.String "Other")), SettingsPage.formMsg Form.Submit ] (loadedWith locked)
                        |> .savingConfig
                        |> Expect.equal Unset
            ]
        , describe "overriding"
            [ test "switching whether the defaults may be overridden is a change" <|
                \_ ->
                    run [ SettingsPage.setOverrideAllowed False ] (loadedWith defaults)
                        |> SettingsPage.isDirty
                        |> Expect.equal True
            , test "switching it back is no change" <|
                \_ ->
                    run [ SettingsPage.setOverrideAllowed False, SettingsPage.setOverrideAllowed True ] (loadedWith defaults)
                        |> SettingsPage.isDirty
                        |> Expect.equal False
            , test "discarding puts the switch back" <|
                \_ ->
                    run [ SettingsPage.setOverrideAllowed False, SettingsPage.discard ] (loadedWith defaults)
                        |> .overrideAllowed
                        |> Expect.equal (Just True)
            , test "a section that says nothing about overriding has no switch to change" <|
                \_ ->
                    run [ SettingsPage.setOverrideAllowed False ] loaded
                        |> .overrideAllowed
                        |> Expect.equal Nothing
            ]
        , describe "loading"
            [ test "the loaded configuration builds the form" <|
                \_ ->
                    SettingsPage.initialModel (Form.initial [] validation)
                        |> run [ SettingsPage.getConfigCompleted (Ok loadedConfig) ]
                        |> .form
                        |> Form.getOutput
                        |> Expect.equal (Just { name = "Organization", links = [ "a", "b" ] })
            , test "a failed load is an error" <|
                \_ ->
                    SettingsPage.initialModel (Form.initial [] validation)
                        |> run [ SettingsPage.getConfigCompleted (Err ApiError.NetworkError) ]
                        |> .config
                        |> ActionResult.isError
                        |> Expect.equal True
            ]
        ]
