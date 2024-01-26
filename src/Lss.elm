module Lss exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
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
    , current : Int
    }


type Mode
    = Expose
    | Presentation


type alias Theme =
    ( Int, String )


type Msg
    = Focus Bool
    | ShowHelp Bool
    | PressedLetter Char
    | PressedControl String
    | OnMarkdownInput String
    | NoOp



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

        ShowHelp b ->
            ( { model | help = b }, Cmd.none )

        PressedControl "Escape" ->
            ( { model | help = False }, Cmd.none )

        PressedControl "ArrowDown" ->
            ( { model | current = next model.current (List.length (Regex.find reExtractSlides model.source)) }, Cmd.none )

        PressedControl "ArrowRight" ->
            ( { model | current = next model.current (List.length (Regex.find reExtractSlides model.source)) }, Cmd.none )

        PressedControl "ArrowUp" ->
            ( { model | current = prev model.current }, Cmd.none )

        PressedControl "ArrowLeft" ->
            ( { model | current = prev model.current }, Cmd.none )

        PressedLetter ' ' ->
            ( { model | sidebarVisible = not model.sidebarVisible }, Cmd.none )

        PressedLetter 'p' ->
            ( { model | mode = cycleMode model.mode }, Cmd.none )

        PressedLetter 't' ->
            ( nextTheme model, Cmd.none )

        PressedLetter 'h' ->
            ( { model | help = True }, Cmd.none )

        PressedControl _ ->
            ( model, Cmd.none )

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
            (generateSlides model.source model.current)
        , if model.help then
            section [ class "Help" ]
                [ h2 [] [ text "Keys definitions" ]
                , dl []
                    [ dt [] [ text "spacebar" ]
                    , dd [] [ text "show/hide editor" ]
                    , dt [] [ text "arrow keys" ]
                    , dd [] [ text "next/previous slide" ]
                    , dt [] [ text "h" ]
                    , dd [] [ text "show/hide this help" ]
                    , dt [] [ text "t" ]
                    , dd [] [ text "cycle through themes" ]
                    , dt [] [ text "p" ]
                    , dd [] [ text "toggle between expose/presentation mode" ]
                    ]
                , button [ class "Help-gotit", onClick (ShowHelp False) ] [ text "Got it" ]
                ]

          else
            text ""
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


generateSlides : String -> Int -> List (Html Msg)
generateSlides md c =
    List.indexedMap
        (\i s ->
            Markdown.toHtml
                [ class "Slide"
                , classList [ ( "is-current", i == c ) ]
                ]
                s
        )
        (Regex.split reExtractSlides md)


reExtractSlides : Regex.Regex
reExtractSlides =
    let
        opts =
            { caseInsensitive = False, multiline = True }

        maybeRegex =
            Regex.fromStringWith opts "^(---|===)$"
    in
    Maybe.withDefault Regex.never maybeRegex


initialModel : Model
initialModel =
    Model False ( 0, "default" ) False True initialSource Expose 0


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


prev : Int -> Int
prev x =
    Basics.max 0 (x - 1)


next : Int -> Int -> Int
next x m =
    Basics.min m (x + 1)


cycleMode : Mode -> Mode
cycleMode m =
    case m of
        Presentation ->
            Expose

        Expose ->
            Presentation


initialSource : String
initialSource =
    """# Lazy Speaker Slides

With **Lazy Speaker Slides** you can write presentations _in seconds_.

Forget the style, focus on the content! :)

Use --- or === to start a new slide.

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
