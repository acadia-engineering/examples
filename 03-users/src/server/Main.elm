module Main exposing (main)


import Dict exposing (Dict)
import Json.Encode as E

import Acadia.Uuid as Uuid
import Assets
import Resolver exposing (Resolver)
import Server exposing (Server, Request, Response, Header)
import Users



-- MAIN


main : Server
main =
  Server.serve <| \req ->
    case req.method of
      "GET" ->
        case req.url of
          "/"        -> ok req Assets.home_elm
          "/profile" -> ok req Assets.profile_elm
          "/signup"  -> ok req Assets.signUp_elm
          "/login"   -> ok req Assets.login_elm
          "/logout"  -> redirect [clearCookie] "/"
          path       ->
            if String.startsWith "/signup/" path
            then trySignup (String.dropLeft 8 path)
            else tryAssets path

      "POST" ->
        Resolver.succeed (Server.proxy "localhost" 9000)

      _ ->
        notFound_



-- RESPONSES


ok : Request -> Assets.Elm -> Resolver Response
ok req elm =
  withSession req <| \info ->
    Resolver.succeed <|
      Server.string 200 [] "text/html" (toElmHtml info elm)


redirect : List Header -> String -> Resolver Response
redirect headers path =
  Resolver.succeed <|
    Server.empty 307 (Server.header "location" path :: headers)


notFound : Resolver Response
notFound =
  Resolver.succeed <|
    Server.string 404 [] "text/html" (toElmHtml Nothing Assets.notFound_elm)


notFound_ : Resolver Response
notFound_ =
  Resolver.succeed <|
    Server.empty 404 []



-- WITH SESSION


withSession : Request -> (Maybe Users.Info -> Resolver a) -> Resolver a
withSession req cont =
  let
    result =
      Dict.get "cookie" req.headers
        |> Maybe.map toCookieDict
        |> Maybe.andThen (Dict.get "u")
        |> Maybe.andThen Uuid.fromBase64
  in
  case result of
    Nothing ->
      cont Nothing

    Just uuid ->
      Resolver.query (Users.lookup (Users.SessionSecret uuid))
        |> Resolver.andThen cont


toCookieDict : String -> Dict String String
toCookieDict cookies =
  let
    segments = String.split ";" cookies
    pairs = List.filterMap toCookiePair segments
  in
  Dict.fromList pairs


toCookiePair : String -> Maybe (String, String)
toCookiePair segment =
  case String.split "=" (String.trim segment) of
    [k,v] -> Just (k,v)
    _     -> Nothing



-- TRY SIGNUP


trySignup : String -> Resolver Response
trySignup secret =
  case Uuid.fromHex secret of
    Just uuid ->
      Resolver.with (Resolver.query (Users.signupDone (Users.EmailSecret uuid))) <| \result ->
        case result of
          Just (info, cookie) ->
            Resolver.succeed <|
              Server.string 200 [setCookie cookie] "text/html" <|
                toElmHtml (Just info) Assets.signUpVerified_elm

          Nothing ->
            notFound

    Nothing ->
      notFound



-- TRY ASSETS


tryAssets : String -> Resolver Response
tryAssets path =
  case (String.left 3 path, String.split "." (String.dropLeft 3 path)) of
    ("/_/", [hash,ext]) ->
      case List.filter (isMatch hash ext) Assets.files of
        [file] -> Resolver.succeed <| Server.file 200 [] { tipe = file.tipe, path = file.path }
        _      -> notFound

    _ -> notFound


isMatch : String -> String -> Assets.File -> Bool
isMatch hash ext file =
  file.hash == hash && file.ext == ext



-- COOKIES


setCookie : Users.SessionSecret -> Header
setCookie (Users.SessionSecret secret) =
  Server.header "set-cookie" <|
    "u=" ++ Uuid.toBase64 secret ++ "; HttpOnly; Path=/; Max-Age=1209600"


clearCookie : Header
clearCookie =
  Server.header "set-cookie" "u=; HttpOnly; Path=/; Max-Age=0"



-- TO ELM HTML


toElmHtml : Maybe Users.Info -> Assets.Elm -> String
toElmHtml info elm =
  """<!DOCTYPE HTML>
<html>
<head>
  <title>Foods</title>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">
  <link rel="preload" href="/_/""" ++ elm.hash ++ """.js" as="script">
</head>
<body>
  <main id="main"></main>
  <script src="/_/""" ++ elm.hash ++ """.js"></script>
  <script>
    Elm.Pages.""" ++ elm.name ++ """.init({
      node: document.getElementById("main"),
      flags: """ ++ E.encode 0 (toFlags info) ++ """
    })</script>
</body>
</html>
"""


toFlags : Maybe Users.Info -> E.Value
toFlags info =
  case info of
    Nothing ->
      E.null

    Just i ->
      let
        (Users.Email email) = i.email
      in
      E.string email


