module Pages.SignUpVerified exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Time



-- MAIN


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { countdown : Int
  }


init : () -> (Model, Cmd Msg)
init () =
  ( { countdown = 5
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = OnTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnTick _ ->
      if model.countdown <= 0
      then
        ( model
        , Nav.load "/profile"
        )
      else
        ( { model | countdown = model.countdown - 1 }
        , Cmd.none
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 1000 OnTick



-- VIEW


view : Model -> Browser.Document msg
view model =
  { title = "Welcome!"
  , body =
      [ h1 [] [ text "Welcome!" ]
      , p []
          [ text <|
              "Redirecting to home page in " ++ String.fromInt model.countdown ++ " seconds."
          ]
      ]
  }


