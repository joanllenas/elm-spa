module Pages.Home exposing (Model, Msg(..), init, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Shared



------------------------------------------------------
--
-- MODEL
--
------------------------------------------------------


type alias Model =
    { count : Int
    , shared : Shared.Model
    }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { count = 0
      , shared = shared
      }
    , Cmd.none
    )



------------------------------------------------------
--
-- UPDATE
--
------------------------------------------------------


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )



------------------------------------------------------
--
-- VIEW
--
------------------------------------------------------


styles : List (Html.Attribute msg)
styles =
    [ Attrs.style "display" "flex"
    , Attrs.style "gap" "10px"
    ]


view : Model -> Html Msg
view model =
    Html.div styles
        [ case model.shared.user of
            Shared.SignedIn username ->
                Html.text <| "Hello " ++ username ++ ": " ++ String.fromInt model.count

            Shared.Guest ->
                Html.text <| "Hello guest: " ++ String.fromInt model.count
        , Html.button [ Events.onClick Increment ] [ Html.text "+" ]
        , Html.button [ Events.onClick Decrement ] [ Html.text "-" ]
        ]
