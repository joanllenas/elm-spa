module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attrs
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


type alias HomeModel =
    { title : String
    }


initHomePage : ( HomeModel, Cmd Msg )
initHomePage =
    ( { title = "Home" }, Cmd.none )



-- SignIn Page


type alias SignInModel =
    { title : String
    }


initSignInPage : ( SignInModel, Cmd Msg )
initSignInPage =
    ( { title = "Sign In" }, Cmd.none )



------------------------------------------------------
--
-- MODEL
--
------------------------------------------------------


type alias Model =
    { navKey : Nav.Key
    , route : Routes.Route
    , page : Page
    }



------------------------------------------------------
--
-- UPDATE
--
------------------------------------------------------


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | Noop


onUrlRequest : Browser.UrlRequest -> Model -> ( Model, Cmd msg )
onUrlRequest urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model
            , Nav.pushUrl model.navKey (Url.toString url)
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
                    Tuple.mapFirst PageHome initHomePage

                Routes.SignInRoute ->
                    Tuple.mapFirst PageSignIn initSignInPage

                Routes.NotFoundRoute ->
                    ( PageNone, Cmd.none )
    in
    ( { model | page = page }, Cmd.batch [ cmd, pageCmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest urlRequest ->
            onUrlRequest urlRequest model

        OnUrlChange url ->
            ( { model | route = Routes.parseUrl url }, Cmd.none )
                |> loadCurrentPage

        Noop ->
            ( model, Cmd.none )



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
    { doc | body = header :: doc.body }



-- Header


headerStyles : List (Html.Attribute msg)
headerStyles =
    [ Attrs.style "display" "flex"
    , Attrs.style "gap" "5px"
    ]


viewHeader : Model -> Html Msg
viewHeader model =
    Html.nav headerStyles
        [ Html.a [ Attrs.href <| Routes.toHref Routes.HomeRoute ] [ Html.text "Home" ]
        , Html.text "|"
        , Html.a [ Attrs.href <| Routes.toHref Routes.SignInRoute ] [ Html.text "Sign In" ]
        ]



-- Home


viewHome : HomeModel -> Browser.Document Msg
viewHome model =
    { title = model.title
    , body =
        [ Html.text "Home!!"
        ]
    }



-- SignIn


viewSignIn : SignInModel -> Browser.Document Msg
viewSignIn model =
    { title = model.title
    , body =
        [ Html.text "Sign In!!"
        ]
    }



------------------------------------------------------
--
-- INIT
--
------------------------------------------------------


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    ( { navKey = navKey
      , route = Routes.parseUrl url
      , page = PageNone
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
