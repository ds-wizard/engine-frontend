module Uuid.Extra exposing
    ( step
    , stepString
    )

import Random exposing (Seed)
import Uuid exposing (Uuid)


step : Seed -> ( Uuid, Seed )
step =
    Random.step Uuid.uuidGenerator


stepString : Seed -> ( String, Seed )
stepString =
    Tuple.mapFirst Uuid.toString << step
