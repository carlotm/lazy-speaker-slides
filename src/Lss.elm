module Lss exposing (main)

-- import Browser.Dom as Dom
-- import File
-- import File.Download as Download
-- import File.Select as Select
-- import Json.Decode as Decode exposing (Decoder, bool, decodeString, field, int, map4, string)
-- import Json.Encode as Encode
-- import Task

import Debug
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onFocus)
import Json.Decode as Decode exposing (string)



----------------------------------------------
-- Types
----------------------------------------------


type alias Model =
    { help : Bool
    , theme : String
    , editorFocused : Bool
    , sidebarVisible : Bool
    , source : String
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

        PressedLetter 't' ->
            ( { model | sidebarVisible = not model.sidebarVisible }, Cmd.none )

        PressedLetter _ ->
            ( model, Cmd.none )



----------------------------------------------
-- View
----------------------------------------------


view : Model -> Html Msg
view model =
    section [ id "Theme", class model.theme ]
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
                [ text model.source ]
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
    Model False "default" False True initialSource


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


themes : List String
themes =
    [ "default", "white", "solarized-dark", "solarized-light" ]

nextTheme : Model -> Model
nextTheme m =
    let
        x = List.foldl (\_ acc -> acc) "default" themes
    in
    m


initialSource : String
initialSource =
    """text before the first title is ignored...

# Lazy Speaker Slides

With **Lazy Speaker Slides** you can write presentations _in seconds_.

Forget the style, focus on the content! :)

Every H1 is a different slide.

# Features

* Various color schemes
* Real time preview
* Emoji support 🤌 😀 🐺

# Code snippets

Code can be inline `rm -r mydir`, or block:

```
PATH='/home/user/code'
cd $PATH
mkdir foo
```

# Keyboard shortcuts

* `spacebar` show/hide editor
* `arrow keys` next/previous slide
* `h` show/hide keyboard shortcuts help
* `t` cycle through themes
* `p` toggle between expose/presentation mode

    """
