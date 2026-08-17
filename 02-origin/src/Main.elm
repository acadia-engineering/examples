module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Backend exposing (Origin(..))
import Acadia.Transaction as Transaction



-- MAIN


main =
  Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }



-- MODEL


type alias Model =
  { food : String
  , origin : String
  , list : List (String, Origin)
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" "" []
  , Transaction.attempt "/_endpoints" Loaded Backend.getFoods
  )



-- UPDATE


type Msg
  = GotFood String
  | GotOrigin String
  | Add
  | Added (Maybe ())
  | Loaded (Maybe (List (String, Origin)))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFood food ->
      ( { model | food = food }
      , Cmd.none
      )

    GotOrigin origin ->
      ( { model | origin = origin }
      , Cmd.none
      )

    Add ->
      ( model
      , Transaction.attempt "/_endpoints" Added (Backend.addFood model.food (toOrigin model.origin))
      )

    Added result ->
      case result of
        Just () ->
          ( { model | food = "", origin = "", list = model.list ++ [ (model.food, toOrigin model.origin) ] }
          , Cmd.none
          )

        Nothing ->
          ( model
          , Cmd.none
          )

    Loaded result ->
      case result of
        Just list ->
          ( { model | list = list }
          , Cmd.none
          )

        Nothing ->
          ( model
          , Cmd.none
          )


toOrigin : String -> Origin
toOrigin origin =
  if String.toLower origin == "local"
  then Local
  else
    if String.toLower origin == "denmark"
    then Denmark
    else Other origin



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Foods (" ++ String.fromInt (List.length model.list) ++ ")"
  , body =
      [ input [ type_ "text", placeholder "Food", value model.food, onInput GotFood ] []
      , input [ type_ "text", placeholder "Origin", value model.origin, onInput GotOrigin ] []
      , input [ type_ "button", value "Add", onClick Add ] []
      , ul [] (List.map viewFood model.list)
      ]
  }


viewFood : (String, Origin) -> Html msg
viewFood (name, origin) =
  let
    location =
      case origin of
        Local   -> "🧑‍🌾"
        Denmark -> "🇩🇰"
        Other o -> o
  in
  li [] [ text <| name ++ " (" ++ location ++ ")" ]


