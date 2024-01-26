module Lss exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onFocus, onInput)
import Json.Decode as Decode exposing (string)
import Markdown
import Regex



----------------------------------------------
-- Types
----------------------------------------------


type alias Model =
    { help : Bool
    , theme : Theme
    , editorFocused : Bool
    , sidebarVisible : Bool
    , source : String
    , mode : Mode
    }


type Mode
    = Expose
    | Presentation


type alias Theme =
    ( Int, String )


type Msg
    = Focus Bool
    | ShowHelp
    | GotHelp
    | NoOp
    | PressedLetter Char
    | PressedControl String
    | OnMarkdownInput String



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

        PressedLetter 'p' ->
            ( { model
                | mode =
                    case model.mode of
                        Presentation ->
                            Expose

                        Expose ->
                            Presentation
              }
            , Cmd.none
            )

        PressedLetter 't' ->
            ( nextTheme model, Cmd.none )

        PressedLetter _ ->
            ( model, Cmd.none )

        OnMarkdownInput v ->
            ( { model | source = v }, Cmd.none )



----------------------------------------------
-- View
----------------------------------------------


view : Model -> Html Msg
view model =
    section [ id "Theme", class (Tuple.second model.theme) ]
        [ aside
            [ classList
                [ ( "Sidebar", True )
                , ( "is-visible", model.sidebarVisible )
                ]
            ]
            [ textarea
                [ class "Sidebar-editor Editor"
                , onInput OnMarkdownInput
                , onFocus (Focus True)
                , onBlur (Focus False)
                , value model.source
                ]
                []
            ]
        , section
            [ classList
                [ ( "Main", True )
                , ( "is-fullw", not model.sidebarVisible )
                , ( "is-presenting", model.mode == Presentation )
                ]
            ]
            (generateSlides model.source)
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


generateSlides : String -> List (Html Msg)
generateSlides md =
    let
        opts =
            { caseInsensitive = True, multiline = True }

        maybeRegex =
            Regex.fromStringWith opts "^(---|===)$"

        regex =
            Maybe.withDefault Regex.never maybeRegex

        slides =
            Regex.split regex md
    in
    List.map (\s -> Markdown.toHtml [ class "Slide" ] s) slides


initialModel : Model
initialModel =
    Model False ( 0, "default" ) False True initialSource Expose


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


themes : List Theme
themes =
    List.indexedMap Tuple.pair [ "default", "white", "solarized-dark", "solarized-light" ]


nextTheme : Model -> Model
nextTheme m =
    let
        currentThemeIdx =
            Tuple.first m.theme

        nextThemeIdx =
            if currentThemeIdx == List.length themes - 1 then
                0

            else
                currentThemeIdx + 1
    in
    case List.filter (\( i, _ ) -> i == nextThemeIdx) themes of
        [ t ] ->
            { m | theme = t }

        _ ->
            m


initialSource : String
initialSource =
    """# Lazy Speaker Slides

With **Lazy Speaker Slides** you can write presentations _in seconds_.

Forget the style, focus on the content! :)

Use hr (---) to define slides.

---

# Features

* Various color schemes
* Real time preview
* Emoji support 🤌 😀 🐺

---

# Code snippets

Code can be inline `rm -r mydir`, or block:

```
PATH='/home/user/code'
cd $PATH
mkdir foo
```

---

# Keyboard shortcuts

* `spacebar` show/hide editor
* `arrow keys` next/previous slide
* `h` show/hide keyboard shortcuts help
* `t` cycle through themes
* `p` toggle between expose/presentation mode

    """
