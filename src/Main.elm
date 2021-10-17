module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode
import Routes
import Url exposing (Url)



------------------------------------------------------
--
-- PAGES
--
------------------------------------------------------


type Page
    = PageNone
    | PageHome HomeModel
    | PageSignIn SignInModel



-- Home Page


type HomeMsg
    = Increment
    | Decrement


type alias HomeModel =
    { count : Int
    , shared : SharedModel
    }


initHomePage : SharedModel -> ( HomeModel, Cmd Msg )
initHomePage shared =
    ( { count = 0
      , shared = shared
      }
    , Cmd.none
    )



-- SignIn Page


type SignInMsg
    = SignIn String
    | SetUsername String


type alias SignInModel =
    { username : String
    , shared : SharedModel
    }


initSignInPage : SharedModel -> ( SignInModel, Cmd Msg )
initSignInPage shared =
    ( { username = ""
      , shared = shared
      }
    , Cmd.none
    )



------------------------------------------------------
--
-- MODEL
--
------------------------------------------------------


type alias Model =
    { route : Routes.Route
    , page : Page
    , shared : SharedModel
    }



-- Shared


type User
    = SignedIn String
    | Guest


type alias SharedModel =
    { user : User
    , navKey : Nav.Key
    }


initShared : Nav.Key -> SharedModel
initShared key =
    { user = Guest
    , navKey = key
    }



------------------------------------------------------
--
-- UPDATE
--
------------------------------------------------------


type SharedMsg
    = SetUser String
    | UnsetUser


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | GotHomeMsg HomeMsg
    | GotSignInMsg SignInMsg
    | GotSharedMsg SharedMsg
    | Noop


onUrlRequest : Browser.UrlRequest -> Model -> ( Model, Cmd msg )
onUrlRequest urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model
            , Nav.pushUrl model.shared.navKey (Url.toString url)
            )

        Browser.External url ->
            ( model
            , Nav.load url
            )


loadCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCurrentPage ( model, cmd ) =
    let
        ( page, pageCmd ) =
            case model.route of
                Routes.HomeRoute ->
                    Tuple.mapFirst PageHome (initHomePage model.shared)

                Routes.SignInRoute ->
                    Tuple.mapFirst PageSignIn (initSignInPage model.shared)

                Routes.NotFoundRoute ->
                    ( PageNone, Cmd.none )
    in
    ( { model | page = page }, Cmd.batch [ cmd, pageCmd ] )

forceUpdate : Msg -> Cmd Msg
forceUpdate msg = Cmd.map (always msg) Cmd.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        executeSharedMsg sharedMsg =
            sharedMsg
                |> Maybe.map (\sharedMsg_ -> updateShared sharedMsg_ model.shared)
                |> Maybe.withDefault model.shared
    in
    case ( model.page, msg ) of
        ( _, OnUrlRequest urlRequest ) ->
            onUrlRequest urlRequest model

        ( _, OnUrlChange url ) ->
            ( { model | route = Routes.parseUrl url }, Cmd.none )
                |> loadCurrentPage

        ( _, GotSharedMsg sharedMsg ) ->
            ({model | shared = executeSharedMsg (Just sharedMsg)}
            , forceUpdate Noop
            ) 

        ( PageHome homeModel, GotHomeMsg homeMsg ) ->
            let
                ( newHomeModel, homeCmd ) =
                    updateHome homeMsg homeModel
            in
            ( { model | page = PageHome newHomeModel }
            , Cmd.map GotHomeMsg homeCmd
            )

        ( PageSignIn signInModel, GotSignInMsg signInMsg ) ->
            let
                ( newSignInModel, signInCmd, sharedMsg ) =
                    updateSignIn signInMsg signInModel 
                newShared = executeSharedMsg sharedMsg
            in
            ( { model
                | page = PageSignIn {newSignInModel | shared = newShared}
                , shared = newShared
              }
            , Cmd.map GotSignInMsg signInCmd
            )

        ( PageNone, _ ) ->
            ( model, Cmd.none )
        
        ( _, _ ) ->
            ( model, Cmd.none )



-- Shared Update


updateShared : SharedMsg -> SharedModel -> SharedModel
updateShared msg model =
    case msg of
        SetUser username ->
            { model | user = SignedIn username }
        UnsetUser ->
            { model | user = Guest }



-- Home Update


updateHome : HomeMsg -> HomeModel -> ( HomeModel, Cmd HomeMsg )
updateHome msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )



-- SignIn Update


updateSignIn : SignInMsg -> SignInModel -> ( SignInModel, Cmd SignInMsg, Maybe SharedMsg )
updateSignIn msg model =
    case msg of
        SignIn username ->
            ( model
            , Routes.replaceUrl model.shared.navKey Routes.HomeRoute
            , Just (SetUser username)
            )

        SetUsername username ->
            ( { model | username = username }, Cmd.none, Nothing )



------------------------------------------------------
--
-- SUBSCRIPTIONS
--
------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



------------------------------------------------------
--
-- VIEW
--
------------------------------------------------------


view : Model -> Browser.Document Msg
view model =
    let
        doc : Browser.Document Msg
        doc =
            case model.page of
                PageHome homeModel ->
                    viewHome homeModel

                PageSignIn signInModel ->
                    viewSignIn signInModel

                PageNone ->
                    { title = "404"
                    , body = [ Html.text "Not found" ]
                    }

        header =
            viewHeader model
    in
    { doc | body = [ Html.div [ Attrs.style "padding" "20px" ] (header :: doc.body) ] }



-- Header


headerStyles : List (Html.Attribute msg)
headerStyles =
    [ Attrs.style "display" "flex"
    , Attrs.style "gap" "5px"
    , Attrs.style "padding-bottom" "10px"
    , Attrs.style "border-bottom" "1px solid #666"
    , Attrs.style "margin-bottom" "10px"
    ]


viewHeader : Model -> Html Msg
viewHeader model =
    Html.nav headerStyles
        [ Html.a [ Attrs.href <| Routes.toHref Routes.HomeRoute ] [ Html.text "Home" ]
        , Html.text "|"
        , loginLink model.shared.user
        ]


loginLink : User -> Html Msg
loginLink user =
    case user of
        SignedIn username ->
            Html.button
                [ Events.onClick <| GotSharedMsg UnsetUser ]
                [ Html.text <| "Sign Out [" ++ username ++ "]" ]

        Guest ->
            Html.a [ Attrs.href <| Routes.toHref Routes.SignInRoute ] [ Html.text "Sign In" ]



-- Home


homeStyles : List (Html.Attribute msg)
homeStyles =
    [ Attrs.style "display" "flex"
    , Attrs.style "gap" "10px"
    ]


viewHome : HomeModel -> Browser.Document Msg
viewHome model =
    { title = "Home"
    , body =
        [ Html.div homeStyles
            [ case model.shared.user of
                SignedIn username ->
                    Html.text <| "Hello " ++ username ++ ": " ++ String.fromInt model.count

                Guest ->
                    Html.text <| "Hello guest: " ++ String.fromInt model.count
            , Html.button [ Events.onClick <| GotHomeMsg Increment ] [ Html.text "+" ]
            , Html.button [ Events.onClick <| GotHomeMsg Decrement ] [ Html.text "-" ]
            ]
        ]
    }



-- SignIn


signInStyles : List (Html.Attribute msg)
signInStyles =
    [ Attrs.style "display" "flex"
    , Attrs.style "gap" "5px"
    ]


viewSignIn : SignInModel -> Browser.Document Msg
viewSignIn model =
    { title = "Sign in"
    , body =
        [ Html.div signInStyles
            [ Html.label [ Attrs.for "username" ]
                [ Html.text "Username"
                ]
            , Html.input
                [ Attrs.id "username", Attrs.type_ "text", Events.onInput (GotSignInMsg << SetUsername) ]
                [ Html.text model.username ]
            , Html.button [ Events.onClick <| GotSignInMsg (SignIn model.username) ] [ Html.text "Sign In" ]
            ]
        ]
    }



------------------------------------------------------
--
-- INIT
--
------------------------------------------------------


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    ( { route = Routes.parseUrl url
      , page = PageNone
      , shared = initShared navKey
      }
    , Cmd.none
    )
        |> loadCurrentPage


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
