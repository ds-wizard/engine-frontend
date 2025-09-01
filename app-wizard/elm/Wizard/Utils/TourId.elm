module Wizard.Utils.TourId exposing
    ( dashboard
    , projectsCreate
    , projectsDetail
    , projectsDetailShareModal
    , projectsIndex
    , usersEditTours
    )

import Shared.Utils.Driver as Driver exposing (TourId)


dashboard : TourId
dashboard =
    Driver.tourId "dashboard"


projectsCreate : TourId
projectsCreate =
    Driver.tourId "projects_create"


projectsDetail : TourId
projectsDetail =
    Driver.tourId "projects_detail"


projectsDetailShareModal : TourId
projectsDetailShareModal =
    Driver.tourId "projects_detail_share-modal"


projectsIndex : TourId
projectsIndex =
    Driver.tourId "projects_index"


usersEditTours : TourId
usersEditTours =
    Driver.tourId "users_edit_tours"
