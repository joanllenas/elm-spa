module Routes exposing (Route(..), parseUrl, replaceUrl, toHref)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = HomeRoute
    | SignInRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map SignInRoute (Parser.s "signin")
        ]


parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchers url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



{--
    example: toHref HomeRoute
-}


toHref : Route -> String
toHref route =
    case route of
        NotFoundRoute ->
            "/not-found"

        SignInRoute ->
            "/signin"

        HomeRoute ->
            "/"


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toHref route)
