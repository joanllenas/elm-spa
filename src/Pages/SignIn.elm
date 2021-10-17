module Pages.SignIn exposing (Model, Msg(..), init, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Routes
import Shared



------------------------------------------------------
--
-- MODEL
--
------------------------------------------------------


type alias Model =
    { username : String
    , shared : Shared.Model
    }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { username = ""
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
    = SignIn String
    | SetUsername String


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Shared.Msg )
update msg model =
    case msg of
        SignIn username ->
            ( model
            , Routes.replaceUrl model.shared.navKey Routes.HomeRoute
            , Just (Shared.SetUser username)
            )

        SetUsername username ->
            ( { model | username = username }, Cmd.none, Nothing )



------------------------------------------------------
--
-- VIEW
--
------------------------------------------------------


styles : List (Html.Attribute msg)
styles =
    [ Attrs.style "display" "flex"
    , Attrs.style "gap" "5px"
    ]


view : Model -> Html Msg
view model =
    Html.div styles
        [ Html.label [ Attrs.for "username" ]
            [ Html.text "Username"
            ]
        , Html.input
            [ Attrs.id "username", Attrs.type_ "text", Events.onInput SetUsername ]
            [ Html.text model.username ]
        , Html.button [ Events.onClick <| SignIn model.username ] [ Html.text "Sign In" ]
        ]
