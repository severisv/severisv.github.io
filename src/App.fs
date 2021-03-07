module App

open Fable.Import
open Fss
open App
open Fable.Core
open Elmish
open Fable.React
open Fable.React.Props
open System

let css = fss >> ClassName

type Guest =
    { Id: Guid
      Name: string
      IsAttending: bool option
      Allergies: string }


let newGuest () =
    { Id = Guid.NewGuid()
      Name = ""
      Allergies = ""
      IsAttending = Option.None }

type Model =
    { Guests: Guest list
      IsSending: bool
      Result: Result<unit, string option> }
    static member createDefault() =
        { Result = Error Option.None
          IsSending = false
          Guests = [ newGuest () ] }


type Msg =
    | SendClicked
    | NameChanged of (Guid * string)
    | AllergiesChanged of (Guid * string)
    | IsAttendingChanged of (Guid * bool)
    | AddGuestClicked
    | SubmitSuccess of string
    | SubmitFailed of exn

let init () = Model.createDefault (), Cmd.none




let updateGuest id fn model =
    { model with
          Guests =
              model.Guests
              |> List.map (function
                  | g when g.Id = id -> fn g
                  | g -> g) }


let downloadAsync () =
    async {
        do! Async.Sleep(2000) // emulate work

        failwithf "Fillern"


        return "networkEmulated"
    }


let update message model =
    match message with
    | SendClicked ->
        Browser.Dom.console.log model
        { model with IsSending = true }, Cmd.OfAsync.either downloadAsync () SubmitSuccess SubmitFailed
    | NameChanged (id, value) -> updateGuest id (fun guest -> { guest with Name = value }) model, Cmd.none
    | AllergiesChanged (id, value) -> updateGuest id (fun guest -> { guest with Allergies = value }) model, Cmd.none
    | IsAttendingChanged (id, value) ->
        updateGuest id (fun guest -> { guest with IsAttending = Some value }) model, Cmd.none
    | AddGuestClicked ->
        { model with
              Guests = model.Guests @ [ newGuest () ] },
        Cmd.none
    | SubmitSuccess _ -> { model with IsSending = false }, Cmd.none
    | SubmitFailed exn ->
        { model with
              IsSending = false
              Result = Error <| Some exn.Message },
        Cmd.none


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



let icon iconName =
    i [ ClassName <| sprintf "fas fa-%s" iconName ] []

let divider iconName =
    div [ css [ MarginTop'(rem 2.0)
                MarginBottom'(rem 1.0) ] ] [
        icon iconName
    ]

let p props =
    p
        (props
         @ [ css [ MarginBottom'(rem 0.0)
                   MarginTop'(rem 0.0)
                   LineHeight'(rem 1.65) ] ])

let h3 (props: CSSProperty list) =
    h3
        ([ css
           <| [ MarginTop'(rem 0.0)
                MarginBottom'(rem 1.0) ]
              @ props ])

let view (model: Model) dispatch =


    div [ css Styles.body ] [
        div [ css [ BackgroundColor' Styles.greenLight
                    TextAlign.Center
                    LetterSpacing'(Functions.em 0.35)
                    Styles.fontFancy
                    MarginBottom'(rem 2.0) ] ] [
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
            div [ css [ MediaQuery [ Media.MaxWidth <| px 600 ] [
                            Display.None
                        ]
                        Display.Flex
                        JustifyContent.Center ] ] [
                div [ css [ TextAlign.Right
                            FlexGrow'(CssFloat 1.0)
                            FlexBasis'(px 0)
                            MarginRight'(rem 1.0) ] ] [
                    h3 [] [ str "24.01.2015" ]
                    p [] [
                        str "Etter å ha unngått hverandre i flere år i Trondhem, møttes Tone og Severin endelig gjennom en felles
                       venn etter at begge hadde flyttet til Oslo. 2 år senere flyttet de sammen i sin første leilighet på
                       Grunerløkka."
                    ]
                ]
                div [ css [ TextAlign.Left
                            FlexGrow'(CssFloat 1.0)
                            FlexBasis'(px 0)
                            MarginLeft'(rem 1.0) ] ] [
                    h3 [] [ str "07.08.2021" ]
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
                            "Det blir ikke mulig å gjennomføre med 1-metersregelen, så hvis den ikke forsvinner må vi skyve på datoen. "
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
                            "Under kan dere svare på om dere kommer eller ikke - skriv eventuelle allergier eller andre behov som kommentar. Svar utbedes innen 1. juni."
                    ]
                    br []
                    br []
                    (match model.Result with
                     | Ok () -> str "Takk"
                     | Error message ->
                         div
                             []
                             ([ div [ css [ Display.Flex
                                            MarginBottom'(rem 0.5) ] ] [
                                 label [ (css
                                              (Styles.label
                                               @ [ Width'(pct 32)
                                                   MarginRight'(rem 1.0) ])) ] [
                                     str "Navn"
                                 ]
                                 label [ (css
                                              (Styles.label
                                               @ [ Width'(pct 32)
                                                   MarginRight'(rem 1.0) ])) ] [
                                     str "Kommentar"
                                 ]
                                 label [ (css (Styles.label @ [ Width'(pct 30) ])) ] [
                                     str "Kommer"
                                 ]
                                ] ]
                              @ (model.Guests
                                 |> List.map (fun g ->
                                     div [ css [ Display.Flex
                                                 AlignItems.Center
                                                 TextAlign.Left
                                                 MarginBottom'(rem 0.5) ] ] [
                                         input [ css
                                                     (Styles.input
                                                      @ [ Width'(pct 32)
                                                          MarginRight'(rem 1.0) ])
                                                 Props.Type "text"
                                                 Value g.Name
                                                 OnChange
                                                 <| fun e -> dispatch <| NameChanged(g.Id, e.Value) ]

                                         input [ css
                                                     (Styles.input
                                                      @ [ Width'(pct 32)
                                                          MarginRight'(rem 1.0) ])
                                                 Props.Type "text"
                                                 Value g.Allergies
                                                 OnChange
                                                 <| fun e -> dispatch <| AllergiesChanged(g.Id, e.Value) ]
                                         div [ css [ Width'(pct 30) ] ] [
                                             button [ css
                                                      <| Styles.button
                                                          Styles.green
                                                             (if g.IsAttending = Some true then
                                                                 Styles.Primary
                                                              else
                                                                  Styles.Secondary)

                                                      OnClick
                                                      <| fun e -> dispatch <| IsAttendingChanged(g.Id, true) ] [
                                                 icon "check"
                                             ]
                                             button [ css
                                                          (Styles.button
                                                              Styles.pink
                                                               (if g.IsAttending = Some false then
                                                                   Styles.Primary
                                                                else
                                                                    Styles.Secondary)
                                                           @ [ MarginLeft'(rem 0.25) ])
                                                      OnClick
                                                      <| fun e -> dispatch <| IsAttendingChanged(g.Id, false) ] [
                                                 icon "times"
                                             ]
                                         ]
                                     ]






                                     ))
                                @ [ div [ css [ TextAlign.Left ] ] [
                                        button [ css Styles.link
                                                 OnClick <| fun _ -> dispatch AddGuestClicked ] [
                                            icon "plus"
                                            str " Legg til gjest"
                                        ]
                                    ]

                                    (message
                                     |> Option.map (fun message ->
                                         p [] [
                                             span [ css [ Color' Styles.pinkDark ] ] [
                                                 str message
                                             ]
                                         ])
                                     |> Option.defaultValue (fragment [] []))

                                    div [ css [ MarginTop'(rem 0.5) ] ] [
                                        button [ Disabled model.IsSending
                                                 css
                                                 <| (Styles.button Styles.blue Styles.Primary)
                                                    @ [ FontSize'(rem 1.0) ]
                                                 OnClick <| fun _ -> dispatch SendClicked ] [
                                            (if model.IsSending then icon "spinner fa-spin" else str "Send")
                                        ]
                                    ] ])

                    )
                ]
                div [] [
                    divider "question"
                    h3 [] [ str "Spørsmål & Svar" ]
                    p [] [ str "Kommer senere!" ]
                ]
                div [] [
                    divider "user-tie"
                    h3 [] [ str "Viktige personer" ]
                    p [] [
                        str "Toastmastere er "
                        a [ css Styles.link
                            Href "https://www.facebook.com/torbjornhar"
                            Target "_blank" ] [
                            str "Torbjørn"
                        ]
                        str " og "
                        a [ css Styles.link
                            Href "https://www.facebook.com/herman.l.hauge"
                            Target "_blank" ] [
                            str "Herman"
                        ]
                        str "."
                    ]
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
