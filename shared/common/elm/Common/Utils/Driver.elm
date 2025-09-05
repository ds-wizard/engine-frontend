port module Common.Utils.Driver exposing
    ( DriverOptionsStep
    , TourConfig
    , TourId
    , addCompletedTourIds
    , addLoggedIn
    , addModalDelay
    , addStep
    , addToursEnabled
    , init
    , onTourDone
    , tourConfig
    , tourId
    )

import Gettext exposing (gettext)
import Json.Encode as E
import Json.Encode.Extra as E
import String.Format as String


type TourId
    = TourId String


tourId : String -> TourId
tourId =
    TourId


type TourConfig
    = TourConfig TourConfigData


type alias TourConfigData =
    { tourId : String
    , toursEnabled : Bool
    , loggedIn : Bool
    , completedTourIds : List String
    , locale : Gettext.Locale
    , steps : List DriverOptionsStep
    , delay : Int
    }


tourConfig : TourId -> Gettext.Locale -> TourConfig
tourConfig (TourId id) locale =
    TourConfig
        { tourId = id
        , toursEnabled = True
        , loggedIn = False
        , completedTourIds = []
        , locale = locale
        , steps = []
        , delay = 0
        }


addToursEnabled : Bool -> TourConfig -> TourConfig
addToursEnabled toursEnabled (TourConfig config) =
    TourConfig { config | toursEnabled = toursEnabled }


addCompletedTourIds : List String -> TourConfig -> TourConfig
addCompletedTourIds completedTourIds (TourConfig config) =
    TourConfig { config | completedTourIds = completedTourIds }


addLoggedIn : Bool -> TourConfig -> TourConfig
addLoggedIn loggedIn (TourConfig config) =
    TourConfig { config | loggedIn = loggedIn }


addStep : DriverOptionsStep -> TourConfig -> TourConfig
addStep step (TourConfig config) =
    TourConfig { config | steps = config.steps ++ [ step ] }


addDelay : Int -> TourConfig -> TourConfig
addDelay delay (TourConfig config) =
    TourConfig { config | delay = delay }


addModalDelay : TourConfig -> TourConfig
addModalDelay =
    addDelay 200


type alias DriverOptionsStep =
    { element : Maybe String
    , popover :
        { title : String
        , description : String
        }
    }


skipTourStr : Gettext.Locale -> String
skipTourStr =
    gettext "Are you sure you want to skip the tour?"


skipTourHint : Gettext.Locale -> String
skipTourHint =
    gettext "You can reset tours in user settings"


encodeTour : TourConfigData -> E.Value
encodeTour config =
    E.object
        [ ( "tourId", E.string config.tourId )
        , ( "steps", E.list encodeStep config.steps )
        , ( "skipTourText", E.string (String.format "%s\n(%s)" [ skipTourStr config.locale, skipTourHint config.locale ]) )
        , ( "nextBtnText", E.string (gettext "Next" config.locale) )
        , ( "prevBtnText", E.string (gettext "Previous" config.locale) )
        , ( "doneBtnText", E.string (gettext "Done" config.locale) )
        , ( "delay", E.int config.delay )
        ]


encodeStep : DriverOptionsStep -> E.Value
encodeStep step =
    E.object
        [ ( "element", E.maybe E.string step.element )
        , ( "popover"
          , E.object
                [ ( "title", E.string step.popover.title )
                , ( "description", E.string step.popover.description )
                ]
          )
        ]


init : TourConfig -> Cmd msg
init (TourConfig tourConfigData) =
    if not tourConfigData.toursEnabled || not tourConfigData.loggedIn || List.member tourConfigData.tourId tourConfigData.completedTourIds then
        Cmd.none

    else
        drive (encodeTour tourConfigData)


port drive : E.Value -> Cmd msg


port onTourDone : (String -> msg) -> Sub msg
