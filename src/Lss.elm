module Lss exposing (main)

-- import Browser.Dom as Dom
-- import File
-- import File.Download as Download
-- import File.Select as Select
-- import Json.Decode as Decode exposing (Decoder, bool, decodeString, field, int, map4, string)
-- import Json.Encode as Encode
-- import Task

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onFocus)
import Json.Decode as Decode exposing (string)



----------------------------------------------
-- Types
----------------------------------------------


type Theme
    = Default
    | White
    | SolarizedDark
    | SolarizedLight


type alias Model =
    { help : Bool
    , theme : Theme
    , editorFocused : Bool
    , sidebarVisible : Bool
    }


type Msg
    = Focus Bool
    | ShowHelp
    | GotHelp
    | NoOp
    | PressedLetter Char
    | PressedControl String



----------------------------------------------
-- Update
----------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Focus b ->
            ( { model | editorFocused = b }, Cmd.none )

        ShowHelp ->
            ( { model | help = True }, Cmd.none )

        GotHelp ->
            ( { model | help = False }, Cmd.none )

        PressedControl _ ->
            ( model, Cmd.none )

        PressedLetter ' ' ->
            ( { model | sidebarVisible = not model.sidebarVisible }, Cmd.none )

        PressedLetter _ ->
            ( model, Cmd.none )



----------------------------------------------
-- View
----------------------------------------------


view : Model -> Html Msg
view model =
    section [ id "Theme", class (themeToString model.theme) ]
        [ aside
            [ classList
                [ ( "Sidebar", True )
                , ( "is-visible", model.sidebarVisible )
                ]
            ]
            [ textarea
                [ class "Sidebar-editor Editor"
                , onFocus (Focus True)
                , onBlur (Focus False)
                ]
                []
            ]
        ]



----------------------------------------------
-- Main
----------------------------------------------


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                if model.editorFocused then
                    Sub.none

                else
                    onKeyDown keyDecoder
        }



----------------------------------------------
-- Helpers
----------------------------------------------


initialModel : Model
initialModel =
    Model False Default False True


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            PressedLetter char

        _ ->
            PressedControl string


themeToString : Theme -> String
themeToString t =
    case t of
        Default ->
            "default"

        White ->
            "white"

        SolarizedDark ->
            "solarized-dark"

        SolarizedLight ->
            "solarized-light"
