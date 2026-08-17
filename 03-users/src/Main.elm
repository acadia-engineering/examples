module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Backend
import Acadia.Transaction as Transaction



-- MAIN
--
-- I do not really like this Elm code!
-- My goal is to highlight the functions in the Backend database module.
-- So please do not take any of this Elm code as a recommendation!


main =
  Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }



-- MODEL


type Model
  = LandingPage { user : String, password : String }
  | ProfilePage { food : String, list : List String }


init : () -> (Model, Cmd Msg)
init _ =
  ( LandingPage { user = "", password = "" }
  , Transaction.attempt "/_endpoints" Loaded Backend.getFoods
  )



-- UPDATE


type Msg
  = GotUser String
  | GotPassword String
  | SignUp
  | LogIn
  | LoggedIn (Maybe ())
  --
  | GotFood String
  | Add
  | Added (Maybe ())
  | Loaded (Maybe (List String))
  | LogOut
  | LoggedOut (Maybe ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotUser user ->
      case model of
        LandingPage s -> ( LandingPage { s | user = user }, Cmd.none )
        ProfilePage _ -> ( model, Cmd.none )

    GotPassword password ->
      case model of
        LandingPage s -> ( LandingPage { s | password = password }, Cmd.none )
        ProfilePage _ -> ( model, Cmd.none )

    SignUp ->
      ( model
      , case model of
          LandingPage s -> Transaction.attempt "/_endpoints" LoggedIn (Backend.addUser s.user s.password)
          ProfilePage _ -> Cmd.none
      )

    LogIn ->
      ( model
      , case model of
          LandingPage s -> Transaction.attempt "/_endpoints" LoggedIn (Backend.login s.user s.password)
          ProfilePage _ -> Cmd.none
      )

    LoggedIn result ->
      case result of
        Just () ->
          ( LandingPage { user = "", password = "" }
          , Transaction.attempt "/_endpoints" Loaded Backend.getFoods
          )

        Nothing ->
          ( model, Cmd.none )

    GotFood food ->
      case model of
        LandingPage _ -> ( model, Cmd.none )
        ProfilePage s -> ( ProfilePage { s | food = food }, Cmd.none )

    Add ->
      case model of
        LandingPage _ -> ( model, Cmd.none )
        ProfilePage s -> ( model, Transaction.attempt "/_endpoints" Added (Backend.addFood s.food) )

    Added result ->
      case result of
        Just () ->
          case model of
            LandingPage _ -> ( model, Cmd.none )
            ProfilePage p -> ( ProfilePage { food = "", list = p.food :: p.list }, Cmd.none )

        Nothing ->
          ( LandingPage { user = "", password = "" }
          , Cmd.none
          )

    Loaded result ->
      case result of
        Just list -> ( ProfilePage { food = "", list = list }, Cmd.none )
        Nothing   -> ( LandingPage { user = "", password = "" }, Cmd.none )

    LogOut ->
      ( model, Transaction.attempt "/_endpoints" LoggedOut Backend.logout )

    LoggedOut result ->
      case result of
        Just () -> ( LandingPage { user = "", password = "" }, Cmd.none )
        Nothing -> ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  case model of
    LandingPage {user,password} ->
      { title = "Log In"
      , body =
          [ input [ type_ "text", placeholder "User Name", value user, onInput GotUser ] []
          , input [ type_ "password", placeholder "Password", value password, onInput GotPassword ] []
          , input [ type_ "button", value "Sign Up", onClick SignUp ] []
          , input [ type_ "button", value "Log In", onClick LogIn ] []
          ]
      }

    ProfilePage {food,list} ->
      { title = "Foods (" ++ String.fromInt (List.length list) ++ ")"
      , body =
          [ input [ type_ "text", placeholder "Food", value food, onInput GotFood ] []
          , input [ type_ "button", value "Add", onClick Add ] []
          , input [ type_ "button", value "Log Out", onClick LogOut ] []
          , ul [] (List.map viewFood list)
          ]
      }


viewFood : String -> Html msg
viewFood name =
  li [] [ text name ]
