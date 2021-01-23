module App

open Fss
open App
open Fable.Core
open Elmish
open Fable.React
open Fable.React.Props

let css = fss >> ClassName

type Model =
    { downloaded: Result<string, string> }
    static member createDefault() = { downloaded = Ok "Ikke noe enda" }

type Msg =
    | OnDownloadClicked
    | ResetClicked
    | OnDownloadedSuccess of string
    | OnDownloadedFail of exn


[<Emit("Math.random()")>]
let getRandom (): float = jsNative

let downloadAsync path =
    async {
        do! Async.Sleep(1000) // emulate work

        let networkEmulated =
            if (getRandom () * 100.) > 50.
            then "Yay! Snart!"
            else raise (System.Exception("Ikke heeelt enda!"))

        return networkEmulated
    }


let init () = Model.createDefault (), Cmd.none

let update message model =
    match message with
    | OnDownloadClicked -> model, Cmd.OfAsync.either downloadAsync "/randomData" OnDownloadedSuccess OnDownloadedFail
    | OnDownloadedSuccess data -> { model with downloaded = Ok data }, Cmd.none
    | OnDownloadedFail ex ->
        { model with
              downloaded = Error ex.Message },
        Cmd.none
    | ResetClicked -> Model.createDefault (), Cmd.none



let io dispatch =
    [ button [ OnClick(fun _ -> dispatch OnDownloadClicked) ] [
        str "Prøv lykken"
      ]
      button [ OnClick(fun _ -> dispatch ResetClicked) ] [
          str "Reset"
      ] ]

let resultView model =
    match model.downloaded with
    | Ok r ->
        h3 [ Style [ CSSProp.Color "blue" ] ] [
            str r
        ]
    | Error er ->
        h3 [ Style [ CSSProp.Color "red" ] ] [
            str er
        ]


type SectionAlignment =
    | Left
    | Right

let section alignment illustration header content =
    let content =
        div [ css <| [ FlexGrow'(CssFloat 1.0) ] ] [
            h3 [ css Styles.h3 ] [ str header ]
            div [] content
        ]

    let margin = div [ css <| [ Width'(rem 3.0) ] ] []

    let illustration =
        div [ css <| [ MaxWidth'(rem 8.0) ] ] [
            illustration
        ]

    div
        [ css [ Display.Flex
                JustifyContent.SpaceBetween
                MarginBottom'(rem 4.0) ] ]
        (match alignment with
         | Left -> [ content; margin; illustration ]
         | Right -> [ illustration; margin; content ])


let view (model: Model) dispatch =


    let illustration url =
        img [ Src url
              css <| [ Width'(pct 100) ] ]

    div [ css Styles.body ] [
        div [ css Styles.container ] [
            h1 [ css
                 <| Styles.h1
                    @ [ TextAlign.Center
                        MarginBottom'(rem 0.50)
                        MarginTop'(rem 3.0) ] ] [
                str "Tone & Severin"
            ]
            h2 [ css
                 <| Styles.h2
                    @ [ TextAlign.Center
                        MarginBottom'(rem 2.0) ] ] [
                str "07.08.21"
            ]
            section Left (illustration "/assets/wedding.svg") "07.08.21" [ str "Info" ]
            section Right (illustration "/assets/corona.svg") "Korona" [ str "Korona" ]
            section
                Left
                (illustration "/assets/transport.svg")
                "Transport og overnatting"
                [ str "Transport og overnatting" ]
            section Right (illustration "/assets/rsvp.svg") "RSVP" [ str "Svar utbedes innen 1. mai" ]
        ]
    ]

open Elmish.React

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
