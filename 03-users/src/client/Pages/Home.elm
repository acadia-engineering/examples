module Pages.Home exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



-- MAIN


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = ()


init : () -> (Model, Cmd Msg)
init _ =
  ( ()
  , Cmd.none
  )



-- UPDATE


type Msg
  = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Foods"
  , body =
      [ h1 [] [ text "Foods" ]
      , p [] [ text "Make lists of foods. View them in different browsers." ]
      , p []
          [ a [ href "/signup" ] [ text "Sign Up" ]
          , text " or "
          , a [ href "/login" ] [ text "Log in" ]
          ]
      ]
  }

