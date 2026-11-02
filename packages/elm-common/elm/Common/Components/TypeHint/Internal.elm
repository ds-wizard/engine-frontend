module Common.Components.TypeHint.Internal exposing
    ( Search
    , accepts
    , failed
    , init
    , isLatest
    , received
    , requested
    , typed
    )

{-| The part of `Common.Components.TypeHint` that decides, kept free of the
DOM and the API so it can be tested: when a request goes out, which response is
kept, and what the field says meanwhile.

Two rules, both about time:

  - **Typing waits.** Every keystroke takes a new tag and starts a timer; only
    the timer carrying the latest tag fetches. A word typed quickly is one
    request, not one per letter.
  - **Only the answer to the current question is kept.** Responses come back in
    whatever order the server finishes them, so a slow answer for "da" can land
    after the answer for "data". It is dropped unless its query is still the
    one in the field.

-}


type alias Search a =
    { tag : Int
    , loading : Bool
    , results : List a
    , error : Maybe String
    }


init : Search a
init =
    { tag = 0
    , loading = False
    , results = []
    , error = Nothing
    }


{-| The user typed. Returns the tag the debounce timer has to carry.

Loading starts now rather than when the request leaves, so the field never
claims "no results" for a query it has not asked about yet.

-}
typed : Search a -> ( Search a, Int )
typed search =
    let
        tag =
            search.tag + 1
    in
    ( { search | tag = tag, loading = True, error = Nothing }, tag )


{-| A request is going out without waiting — the popup has just opened.
Takes a tag too, so a timer still running from before is superseded.
-}
requested : Search a -> Search a
requested search =
    { search | tag = search.tag + 1, loading = True, error = Nothing }


{-| Whether a timer that has just fired is the latest one, and so should fetch.
-}
isLatest : Int -> Search a -> Bool
isLatest tag search =
    tag == search.tag


{-| Whether a response is for the query in the field now.
-}
accepts : { response : String, current : String } -> Bool
accepts { response, current } =
    response == current


{-| Results for the current query arrived. The old ones stay visible until
this point, so the list does not empty and refill on every keystroke.
-}
received : List a -> Search a -> Search a
received results search =
    { search | loading = False, results = results, error = Nothing }


failed : String -> Search a -> Search a
failed message search =
    { search | loading = False, results = [], error = Just message }
