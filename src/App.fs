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


let view (model: Model) dispatch =


    let illustration url =
        img [ Src url
              css <| [ Width'(pct 100) ] ]

    div [ css Styles.body ] [
        div [ css Styles.container ] [
            div [ css [ BackgroundColor' Styles.greenLight; TextAlign.Center; Height' (rem 25.0) ] ] [
                h2 [ ] [
                    str "Tone & Severin"
                ]
                h1 [] [
                    str "Vi gifter oss!"  
                ]
                h3 [] [
                    str "07.08.2021"
                ]
            ]
            div [ css [Display.Flex
                       JustifyContent.Center ] ] [
               div [css [TextAlign.Right
                         FlexGrow' (CssFloat 1.0)
                         MarginRight' (rem 1.0)]] [
                   h3 [] [
                       str "24.01.2015"
                   ]
                   p [] [
                       str "Etter å ha unngått hverandre i flere år i Trondhem, møttes Tone & Severin endelig gjennom en av mange felles
                       venner først etter at begge hadde byttet ut Trøndelag med hovedstaden. 2 år senere flyttet de sammen i sin første leilighet på
                       Grunerløkka."
                   ]
                   p [] [
                        str "Bare 2 år senere flyttet de 50 meter bort i gaten, og i denne leiligheten forlovet de seg en sen sommer dag i september 2020."
                    ]
               ]
               div [css [TextAlign.Left
                         FlexGrow' (CssFloat 1.0)
                         MarginLeft' (rem 1.0)]] [
                   h3 [] [
                       str "07.08.2021"
                   ]
                   p [] [
                       str "Vi planlegger gifte seg i den flotte måneden august 2021 - sett at Bent og Erna tillater store feiringer.
                       Vi gleder oss til den store dagen sammen med venner og familie, og med hverandre."
                   ]
                   p [] [
                       str "Planen for dagen formes, og denne nettsiden vil holde deg oppdatert med alt du trenger å vite."
                   ]
               ]
            ]
            div [css [TextAlign.Center; MaxWidth' (rem 32.0); Margin.Auto]] [
                div [] [
                    div [] [
                      str "&hearts;"                   
                    ]
                    h3 [] [
                        str "Om bryllupet"
                    ]
                    p [] [
                        str "Festen avholdes 7.auggust 2021 på Folkvang, Drøbak (Sevs, insert link). Informasjon om seremoni og tider kommer senere."
                    ]
                ]
                div [] [
                    div [] [
                       str "&virus;"                   
                    ]
                    h3 [] [
                        str "Om korona"
                    ]
                    p [] [
                        str "Bryllupet vil dessverre ikke kunne avholdes med korona-restriksjoner som 1-metersreegelen. Vi vil holde dere oppdatert her
                        ettersom det nærmer seg, men vi håper på at vaksinen både er god og godt distribuert innen august. Pass på å ikke bestill reise eller
                        overnatting som ikke kan avbestilles, slik situasjonen er nå kan vi ikke si sikkert om bryllupet blir usatt eller ikke."
                    ]
                ]
                div [] [
                    div [] [
                       str "&house;"                   
                    ]
                    h3 [] [
                        str "Om overnatting"
                    ]
                    p [] [
                        str "Det finnes overnatting både i Oslo og Drøbak - Folkvang er bare en halvtimes kjøretur fra Oslo og ligger fint til
                        bussforbindelser. Velg det som passer deg best! Hvis interessen for det er stor, vil vi muliigens sitte opp busser fra festen til
                        Oslo sentrum."
                    ]
                ]
                div [] [
                    div [] [
                       str "&house;"                   
                    ]
                    h3 [] [
                        str "Om overnatting"
                    ]
                    p [] [
                        str "Det finnes overnatting både i Oslo og Drøbak - Folkvang er bare en halvtimes kjøretur fra Oslo og ligger fint til
                        bussforbindelser. Velg det som passer deg best! Hvis interessen for det er stor, vil vi muliigens sitte opp busser fra festen til
                        Oslo sentrum."
                    ]
                ]
                div [] [
                    div [] [
                       str "&envelope;"                   
                    ]
                    h3 [] [
                        str "RVSP"
                    ]
                    p [] [
                        str "Kommer senere!"
                    ]
                ]
                div [] [
                    div [] [
                       str "&qa;"                   
                    ]
                    h3 [] [
                        str "Spørsmål & Svar"
                    ]
                    p [] [
                        str "Kommer senere!"
                    ]
                ]
                div [] [
                    div [] [
                       str "&boss;"                   
                    ]
                    h3 [] [
                        str "Viktige person"
                    ]
                    p [] [
                        str "Kommer senere!"
                    ]
                ]
                div [] [
                    div [] [
                       str "&gift;"                   
                    ]
                    h3 [] [
                        str "Ønskeliste"
                    ]
                    p [] [
                        str "Kommer senere!"
                    ]
                ]
            ]
            // old

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
            section
                Right
                (illustration "/assets/corona.svg")
                "Korona"
                [ str "Korona\n"
                  br []
                  p [ css [ MarginTop'(rem 5.0) ] ] [
                      str " ihi"
                  ]
                  str "hola"
                  p [ css [ TextAlign.Right
                            TextAlign.Center
                            TextAlign.End ] ] [
                      str "Jåv"
                  ] ]
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
