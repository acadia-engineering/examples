module Pages.SignUp exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Acadia.Transaction as Transaction
import Acadia.Uuid as Uuid
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
--
-- Normally you would send the EmailSecret via email. Setting up an email
-- service on the server is outside the scope of this example, so we just
-- return the email secret directly to get the point across.


type alias Model =
  { email : String
  , password : String
  , password2 : String
  , secret : Maybe Users.EmailSecret
  }


init : Maybe String -> (Model, Cmd Msg)
init flags =
  ( { email = ""
    , password = ""
    , password2 = ""
    , secret = Nothing
    }
  , case flags of
      Nothing -> Cmd.none
      Just _  -> Nav.load "/"
  )



-- UPDATE


type Msg
  = GotEmail String
  | GotPassword1 String
  | GotPassword2 String
  | SignUp
  | SignedUp (Maybe Users.EmailSecret)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotEmail email ->
      ( { model | email = email }, Cmd.none )

    GotPassword1 password ->
      ( { model | password = password }, Cmd.none )

    GotPassword2 password ->
      ( { model | password2 = password }, Cmd.none )

    SignUp ->
      ( model
      , if isOkayPassword model.password model.password2
        then
          Transaction.attempt "/_endpoints" SignedUp <|
            Users.signupInit (Users.Email model.email) (Users.Password model.password)
        else Cmd.none
      )

    SignedUp result ->
      ( { model | secret = result }
      , Cmd.none
      )


-- You can do better checks here!
--
isOkayPassword : String -> String -> Bool
isOkayPassword password password2 =
  not (String.isEmpty password) && password == password2



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Sign Up"
  , body =
      [ input [ type_ "text", placeholder "Email", value model.email, onInput GotEmail ] []
      , input [ type_ "password", placeholder "Password", value model.password, onInput GotPassword1 ] []
      , input [ type_ "password", placeholder "Retype Password", value model.password2, onInput GotPassword2 ] []
      , input [ type_ "button", value "Sign Up", disabled (not (isOkayPassword model.password model.password2)), onClick SignUp ] []
      , p []
          [ a [ href "/" ] [ text "Home" ]
          , text " "
          , a [ href "/login" ] [ text "Log In" ]
          ]
      , case model.secret of
          Nothing -> text ""
          Just (Users.EmailSecret secret) ->
            let
              path = "/signup/" ++ Uuid.toHex secret
            in
            p []
              [ text "An email verification session has been created. Integrating with an email sending service is outside the scope of this example, so just pretend that you got this verification link through your email: "
              , a [ href path ] [ text path ]
              ]
      ]
  }
