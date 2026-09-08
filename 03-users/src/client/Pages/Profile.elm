module Pages.Profile exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Foods
import Acadia.Transaction as Transaction



-- MAIN


main : Program (Maybe String) Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { food : String
  , origin : String
  , list : List Foods.Info
  }


init : Maybe String -> (Model, Cmd Msg)
init flags =
  ( { food = ""
    , origin = ""
    , list = []
    }
  , case flags of
      Nothing -> Nav.load "/signup"
      Just _  -> Transaction.attempt "/_endpoints" Loaded Foods.getFoods
  )



-- UPDATE


type Msg
  = GotFood String
  | GotOrigin String
  | Add
  | Added (Maybe ())
  | Loaded (Maybe (List Foods.Info))


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
      , Transaction.attempt "/_endpoints" Added (Foods.addFood model.food (toOrigin model.origin))
      )

    Added result ->
      case result of
        Just () ->
          ( { model | food = "", origin = "", list = model.list ++ [ Foods.Info model.food (toOrigin model.origin) ] }
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


toOrigin : String -> Foods.Origin
toOrigin origin =
  if String.toLower origin == "local"
  then Foods.Local
  else
    if String.toLower origin == "denmark"
    then Foods.Denmark
    else Foods.Other origin



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Foods (" ++ String.fromInt (List.length model.list) ++ ")"
  , body =
      [ input [ type_ "text", placeholder "Food", value model.food, onInput GotFood ] []
      , input [ type_ "text", placeholder "Origin", value model.origin, onInput GotOrigin ] []
      , input [ type_ "button", value "Add", onClick Add ] []
      , ul [] (List.map viewFood model.list)
      , p [] [ a [ href "/logout" ] [ text "Log Out" ] ]
      ]
  }


viewFood : Foods.Info -> Html msg
viewFood food =
  let
    location =
      case food.origin of
        Foods.Local   -> "🧑‍🌾"
        Foods.Denmark -> "🇩🇰"
        Foods.Other o -> o
  in
  li [] [ text <| food.name ++ " (" ++ location ++ ")" ]


