module Shared exposing (Model, Msg(..), User(..), init, update)

import Browser.Navigation as Nav



------------------------------------------------------
--
-- MODEL
--
------------------------------------------------------


type User
    = SignedIn String
    | Guest


type alias Model =
    { user : User
    , navKey : Nav.Key
    }


init : Nav.Key -> Model
init key =
    { user = Guest
    , navKey = key
    }



------------------------------------------------------
--
-- UPDATE
--
------------------------------------------------------


type Msg
    = SetUser String
    | UnsetUser


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetUser username ->
            { model | user = SignedIn username }

        UnsetUser ->
            { model | user = Guest }
