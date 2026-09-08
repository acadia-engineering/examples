module Pages.Login exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Acadia.Transaction as Transaction
import Users



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
  { email : String
  , password : String
  }


init : Maybe String -> (Model, Cmd Msg)
init flags =
  ( { email = ""
    , password = ""
    }
  , case flags of
      Nothing -> Cmd.none
      Just _  -> Nav.load "/"
  )



-- UPDATE


type Msg
  = GotEmail String
  | GotPassword String
  | Login
  | LoggedIn (Maybe ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotEmail email ->
      ( { model | email = email }, Cmd.none )

    GotPassword password ->
      ( { model | password = password }, Cmd.none )

    Login ->
      ( model
      , Transaction.attempt "/_endpoints" LoggedIn <|
          Users.login (Users.Email model.email) (Users.Password model.password)
      )

    LoggedIn result ->
      case result of
        Nothing ->
          ( { email = ""
            , password = ""
            }
          , Cmd.none
          )

        Just () ->
          ( model
          , Nav.load "/profile"
          )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Log In"
  , body =
      [ input [ type_ "text", placeholder "Email", value model.email, onInput GotEmail ] []
      , input [ type_ "password", placeholder "Password", value model.password, onInput GotPassword ] []
      , input [ type_ "button", value "Log In", onClick Login ] []
      , p []
          [ a [ href "/" ] [ text "Home" ]
          , text " "
          , a [ href "/signup" ] [ text "Sign Up" ]
          ]
      ]
  }
