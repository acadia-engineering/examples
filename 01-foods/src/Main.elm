module Main exposing (main)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Backend
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
  , list : List String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" []
  , Transaction.attempt "/_endpoints" Loaded Backend.getFoods
  )



-- UPDATE


type Msg
  = GotFood String
  | Add
  | Added (Maybe ())
  | Loaded (Maybe (List String))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "msg" msg of
    GotFood food ->
      ( { model | food = food }
      , Cmd.none
      )

    Add ->
      ( model
      , Transaction.attempt "/_endpoints" Added (Backend.addFood model.food)
      )

    Added result ->
      case result of
        Just () ->
          ( { model | food = "", list = model.list ++ [ model.food ] }
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



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Foods (" ++ String.fromInt (List.length model.list) ++ ")"
  , body =
      [ input [ type_ "text", placeholder "Food", value model.food, onInput GotFood ] []
      , input [ type_ "button", value "Add", onClick Add ] []
      , ul [] (List.map viewFood model.list)
      ]
  }


viewFood : String -> Html msg
viewFood name =
  li [] [ text name ]
