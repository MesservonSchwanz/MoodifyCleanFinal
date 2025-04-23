// Hangulatnapló alkalmazás - Projekt Alpha (2025 tavasz)
// Készítette: Hallgató (Neptun kód: ABC123)
// Verzió: 2025.04.23

module Main

open Feliz
open Elmish
open Elmish.React
open Thoth.Json

// ----------------------------------------
// Típusdefiníciók
// ----------------------------------------

type Theme =
    | Light
    | Dark

type Mood =
    { Rating: int
      Note: string
      Timestamp: System.DateTime }

type Model =
    { CurrentMood: Mood
      SavedMoods: Mood list
      Theme: Theme }

type Msg =
    | SetRating of int
    | SetNote of string
    | SaveMood
    | ToggleTheme

// ----------------------------------------
// LocalStorage mentés/betöltés
// ----------------------------------------

let loadMoods (): Mood list =
    match Browser.Dom.window.localStorage.getItem("moods") with
    | null -> []
    | json -> Decode.Auto.unsafeFromString<Mood list> json

let saveMoods (moods: Mood list) =
    let json = Encode.Auto.toString(2, moods)
    Browser.Dom.window.localStorage.setItem("moods", json)

// ----------------------------------------
// Inicializáció
// ----------------------------------------

let init () : Model * Cmd<Msg> =
    { CurrentMood = { Rating = 3; Note = ""; Timestamp = System.DateTime.Now }
      SavedMoods = loadMoods()
      Theme = Light },
    Cmd.none

// ----------------------------------------
// Állapot frissítése
// ----------------------------------------

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetRating r -> 
        { model with CurrentMood = { model.CurrentMood with Rating = r } }, Cmd.none
    | SetNote text ->
        { model with CurrentMood = { model.CurrentMood with Note = text } }, Cmd.none
    | SaveMood ->
        let updated = { model.CurrentMood with Timestamp = System.DateTime.Now } :: model.SavedMoods
        saveMoods updated
        { model with 
            SavedMoods = updated
            CurrentMood = { Rating = 3; Note = ""; Timestamp = System.DateTime.Now } },
        Cmd.none
    | ToggleTheme ->
        let newTheme = if model.Theme = Light then Dark else Light
        { model with Theme = newTheme }, Cmd.none

// ----------------------------------------
// UI komponensek
// ----------------------------------------

let moodButton (i: int) (current: int) (dispatch: Msg -> unit) =
    Html.button [
        prop.text (string i)
        prop.onClick (fun _ -> dispatch (SetRating i))
        prop.style [
            style.margin (length.rem 0.25)
            style.padding (length.rem 0.5)
            style.backgroundColor (if i = current then "#90cdf4" else "#e2e8f0")
            style.borderRadius (length.px 5)
            style.border (1, borderStyle.solid, "#ccc")
            style.cursor.pointer
        ]
    ]

let themedContainer (theme: Theme) (children: ReactElement list) =
    let bg = if theme = Dark then "#1a202c" else "#f7fafc"
    let fg = if theme = Dark then "white" else "black"

    Html.div [
        prop.style [
            style.backgroundColor bg
            style.color fg
            style.minHeight (length.vh 100)
            style.padding (length.rem 2)
            style.fontFamily "Segoe UI, sans-serif"
            style.display.flex
            style.justifyContent.center
            style.alignItems.center
            style.flexDirection.column
        ]
        prop.children children
    ]

// ----------------------------------------
// Nézet (renderelés)
// ----------------------------------------

let view (model: Model) (dispatch: Msg -> unit) =
    themedContainer model.Theme [

        Html.h1 [
            prop.text "Moodify 😊"
            prop.style [ style.fontSize (length.em 2); style.marginBottom (length.rem 1) ]
        ]

        Html.button [
            prop.text (if model.Theme = Dark then "Világos mód" else "Sötét mód")
            prop.onClick (fun _ -> dispatch ToggleTheme)
            prop.style [
                style.marginBottom (length.rem 1)
                style.padding (length.rem 0.5)
                style.borderRadius (length.px 4)
                style.border (1, borderStyle.solid, "transparent")
                style.backgroundColor "#718096"
                style.color "white"
                style.cursor.pointer
            ]
        ]

        Html.p [ prop.text "Hogyan érzed magad (1–5)?" ]

        Html.div [
            prop.style [ style.display.flex; style.marginBottom (length.rem 1) ]
            prop.children [ for i in 1..5 -> moodButton i model.CurrentMood.Rating dispatch ]
        ]

        Html.div [
            Html.p [ prop.text "Megjegyzés:" ]
            Html.textarea [
                prop.value model.CurrentMood.Note
                prop.onChange (SetNote >> dispatch)
                prop.rows 3
                prop.style [
                    style.width (length.percent 100)
                    style.marginBottom (length.rem 1)
                    style.padding (length.rem 0.5)
                    style.borderRadius (length.px 4)
                    style.border (1, borderStyle.solid, "#cbd5e0")
                ]
            ]
        ]

        Html.button [
            prop.text "Mentés"
            prop.onClick (fun _ -> dispatch SaveMood)
            prop.style [
                style.padding (length.rem 0.5)
                style.borderRadius (length.px 4)
                style.border (1, borderStyle.solid, "transparent")
                style.backgroundColor "#48bb78"
                style.color "white"
                style.cursor.pointer
            ]
        ]

        Html.hr [ prop.style [ style.marginTop (length.rem 2); style.marginBottom (length.rem 1); style.width (length.percent 100) ] ]

        Html.h2 [ prop.text "Korábbi hangulatok:" ]

        Html.ul [
            for m in model.SavedMoods ->
                Html.li [
                    prop.text (sprintf "%d/5 (%s) – %s" m.Rating (m.Timestamp.ToShortTimeString()) m.Note)
                    prop.style [ style.marginBottom (length.rem 0.5) ]
                ]
        ]
    ]

// ----------------------------------------
// Program indítás
// ----------------------------------------

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.run