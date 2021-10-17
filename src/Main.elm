module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode
import Pages.Home
import Pages.SignIn
import Routes
import Shared
import Url exposing (Url)



------------------------------------------------------
--
-- PAGES
--
------------------------------------------------------


type Page
    = PageNone
    | PageHome Pages.Home.Model
    | PageSignIn Pages.SignIn.Model



------------------------------------------------------
--
-- MODEL
--
------------------------------------------------------


type alias Model =
    { route : Routes.Route
    , page : Page
    , shared : Shared.Model
    }



------------------------------------------------------
--
-- UPDATE
--
------------------------------------------------------


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | GotHomeMsg Pages.Home.Msg
    | GotSignInMsg Pages.SignIn.Msg
    | GotSharedMsg Shared.Msg
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
                    Tuple.mapFirst PageHome (Pages.Home.init model.shared)
                        |> Tuple.mapSecond (Cmd.map GotHomeMsg)

                Routes.SignInRoute ->
                    Tuple.mapFirst PageSignIn (Pages.SignIn.init model.shared)
                        |> Tuple.mapSecond (Cmd.map GotSignInMsg)

                Routes.NotFoundRoute ->
                    ( PageNone, Cmd.none )
    in
    ( { model | page = page }, Cmd.batch [ cmd, pageCmd ] )


forceUpdate : Msg -> Cmd Msg
forceUpdate msg =
    Cmd.map (always msg) Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        executeSharedMsg sharedMsg =
            sharedMsg
                |> Maybe.map (\sharedMsg_ -> Shared.update sharedMsg_ model.shared)
                |> Maybe.withDefault model.shared
    in
    case ( model.page, msg ) of
        ( _, OnUrlRequest urlRequest ) ->
            onUrlRequest urlRequest model

        ( _, OnUrlChange url ) ->
            ( { model | route = Routes.parseUrl url }, Cmd.none )
                |> loadCurrentPage

        ( _, GotSharedMsg sharedMsg ) ->
            ( { model | shared = executeSharedMsg (Just sharedMsg) }
            , forceUpdate Noop
            )

        ( PageHome homeModel, GotHomeMsg homeMsg ) ->
            let
                ( newHomeModel, homeCmd ) =
                    Pages.Home.update homeMsg homeModel
            in
            ( { model | page = PageHome newHomeModel }
            , Cmd.map GotHomeMsg homeCmd
            )

        ( PageSignIn signInModel, GotSignInMsg signInMsg ) ->
            let
                ( newSignInModel, signInCmd, sharedMsg ) =
                    Pages.SignIn.update signInMsg signInModel

                newShared =
                    executeSharedMsg sharedMsg
            in
            ( { model
                | page = PageSignIn { newSignInModel | shared = newShared }
                , shared = newShared
              }
            , Cmd.map GotSignInMsg signInCmd
            )

        ( PageNone, _ ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
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
                    { title = "Home"
                    , body = [ Html.map GotHomeMsg (Pages.Home.view homeModel) ]
                    }

                PageSignIn signInModel ->
                    { title = "Sign In"
                    , body = [ Html.map GotSignInMsg (Pages.SignIn.view signInModel) ]
                    }

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


loginLink : Shared.User -> Html Msg
loginLink user =
    case user of
        Shared.SignedIn username ->
            Html.button
                [ Events.onClick <| GotSharedMsg Shared.UnsetUser ]
                [ Html.text <| "Sign Out [" ++ username ++ "]" ]

        Shared.Guest ->
            Html.a [ Attrs.href <| Routes.toHref Routes.SignInRoute ] [ Html.text "Sign In" ]



------------------------------------------------------
--
-- INIT
--
------------------------------------------------------


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    ( { route = Routes.parseUrl url
      , page = PageNone
      , shared = Shared.init navKey
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
