module Wizard.Pages.Comments.Models exposing (Model, initialModel)

import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Components.Listing.Models as Listing
import Wizard.Routes exposing (commentsRouteResolvedFilterId)


type alias Model =
    { commentThreads : Listing.Model QuestionnaireCommentThreadAssigned
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbResolved =
    let
        values =
            [ ( commentsRouteResolvedFilterId, mbResolved )
            ]

        paginationQueryFilters =
            PaginationQueryFilters.create values []
    in
    { commentThreads = Listing.initialModelWithFilters paginationQueryString paginationQueryFilters
    }
