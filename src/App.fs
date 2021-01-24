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



let init () = Model.createDefault (), Cmd.none

let update message model = Model.createDefault (), Cmd.none


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



let divider iconName =
    div [ css [ MarginTop'(rem 2.0)
                MarginBottom'(rem 1.0) ] ] [
        i [ ClassName <| sprintf "fas fa-%s" iconName ] []
    ]

let p props =
    p
        (props
         @ [ css [ MarginBottom'(rem 0.0)
                   MarginTop'(rem 0.0)
                   LineHeight'(rem 1.65) ] ])

let h3 (props: CSSProperty list) =
    h3
        ([ css <|[ MarginTop'(rem 0.0)
                   MarginBottom'(rem 1.0) ] @ props])

let view (model: Model) dispatch =


    div [ css Styles.body ] [
        div [ css [ BackgroundColor' Styles.greenLight
                    TextAlign.Center
                    LetterSpacing'(Functions.em 0.35)
                    Styles.fontFancy
                    MarginBottom' (rem 2.0)
                     ] ] [
            h2 [ css [ Color' Styles.green ] ] [
                str "TONE & SEVERIN"
            ]
            div [ ClassName
                  <| "headerBackground " + fss [ Height'(rem 23.0) ] ] [
                h1 [ css [ MarginTop'(px 180) ] ] [
                    str "VI GIFTER OSS!"
                ]
                h3 [ MarginTop'(rem 0.0) ] [
                    str "7.AUGUST 2021, FOLKVANG, DRØBAK"
                ]
            ]
        ]
        div [ css Styles.container ] [
            div [ css [
                        MediaQuery [Media.MaxWidth <| px 600] [Display.None]
                        Display.Flex
                        JustifyContent.Center ] ] [
                div [ css [ TextAlign.Right
                            FlexGrow'(CssFloat 1.0)
                            FlexBasis' (px 0)
                            MarginRight'(rem 1.0) ] ] [
                    h3 [] [
                       str "24.01.2015"
                    ]
                    p [] [
                        str "Etter å ha unngått hverandre i flere år i Trondhem, møttes Tone og Severin endelig gjennom en felles
                       venn etter at begge hadde flyttet til Oslo. 2 år senere flyttet de sammen i sin første leilighet på
                       Grunerløkka."
                    ]
                ]
                div [ css [ TextAlign.Left
                            FlexGrow'(CssFloat 1.0)
                            FlexBasis' (px 0)
                            MarginLeft'(rem 1.0) ] ] [
                    h3 [] [
                       str "07.08.2021"
                    ]
                    p [] [
                        str "Vi planlegger å gifte oss i august 2021 - gitt at koronaviruset jekker seg ned litt."
                    ]
                    p [] [
                        str "Planen for dagen formes, og denne nettsiden vil oppdateres med nyttig informasjon."
                    ]
                ]
            ]
            div [ css [ TextAlign.Center
                        MaxWidth'(rem 32.0)
                        Margin.Auto ] ] [
                div [] [
                    divider "heart"
                    h3 [] [ str "Bryllupet" ]
                    p [] [
                        str "Festen avholdes 7.august 2021 på det tradisjonsrike grendehuset i Frogn, Folkvang."
                        br []
                        str "Informasjon om seremoni og tider kommer senere."
                    ]
                ]
                div [] [
                    divider "virus"
                    h3 [] [ str "Korona" ]
                    p [] [
                        str
                            "Det blir ikke mulig å gjennomføre med 1-metersregelen, så hvis den ikke forsvinner må vi skyve på datoen."
                        str "Vi vil vurdere en eventuell utsettelse i god tid i forveien, så ikke bestill noen reise eller overnatting før vi kan si med sikkerhet
                        om det lar  seg gjennomføre til planlagt tid.
                        "
                    ]
                ]
                div [] [
                    divider "home"
                    h3 [] [ str "Overnatting" ]
                    p [] [
                        str "Det finnes overnatting både i Oslo og Drøbak - Folkvang er bare en halvtimes kjøretur fra Oslo og ligger fint til
                        bussforbindelser. Velg det som passer deg best! Hvis interessen er stor, kan det hende vi ordner en egen buss fra festen til
                        Oslo sentrum."
                    ]
                ]
                div [] [
                    divider "envelope"
                    h3 [] [ str "RVSP" ]
                    p [] [
                        str
                            "Det kommer et skjema her på siden hvor man kan svare og fylle inn litt allergier og denslags."
                    ]
                ]
                div [] [
                    divider "question"
                    h3 [] [ str "Spørsmål & Svar" ]
                    p [] [ str "Kommer senere!" ]
                ]
                div [] [
                    divider "user-tie"
                    h3 [] [ str "Viktige personer" ]
                    p [] [ str "Kommer senere!" ]
                ]
                div [] [
                    divider "gift"
                    h3 [] [ str "Ønskeliste" ]
                    p [] [ str "Kommer senere!" ]
                ]
            ]
        ]
    ]

open Elmish.React

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
