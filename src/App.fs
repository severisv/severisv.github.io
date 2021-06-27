module App

open Fable.Import
open Fable.SimpleHttp
open Fss
open App
open Fable.Core
open Elmish
open Fable.React
open Fable.React.Props
open System
open Utils

let YXp6 =
    [ "QjFYRFE0VTBH"
      "c2xhY2suY29t"
      "NjdYYVpEbUYyV0RXcU03ZUpaT2E4U3NH"
      "aHR0cHM="
      "VDAyQTU0QTAz" ]

let css = fss >> ClassName

type Guest =
    { Id: Guid
      Name: string
      IsAttending: bool option
      WantsBus: bool
      Allergies: string }


let newGuest () =
    { Id = Guid.NewGuid()
      Name = ""
      Allergies = ""
      IsAttending = Option.None
      WantsBus = false }

type Model =
    { Guests: Guest list
      IsSending: bool
      ValidationMessage: string option
      Result: Result<unit, string option> }
    static member createDefault() =
        { Result = Error Option.None
          IsSending = false
          ValidationMessage = Option.None
          Guests = [ newGuest () ] }


type Msg =
    | SendClicked
    | NameChanged of (Guid * string)
    | AllergiesChanged of (Guid * string)
    | IsAttendingChanged of (Guid * bool)
    | WantBusChanged of Guid
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


let url =
    let YXp6 = YXp6 |> List.map Z2dnZzI

    
    sprintf
        "%s://%s.%s/services/%s/%s/%s"
        YXp6.[3]
        ("h1337ks".Replace("1337", "oo"))
        YXp6.[1]
        YXp6.[4]
        YXp6.[0]
        YXp6.[2]

let downloadAsync (guests: Guest list) =
    async {

        let guests =
            guests
            |> List.map (fun g ->
                sprintf
                    "{\"title\":\"%s\",\"value\":\"%s - %s        Buss: %s\"}"
                    g.Name
                    (g.IsAttending
                     |> Option.map (function
                         | true -> "Kommer"
                         | false -> "Kommer ikke")
                     |> Option.defaultValue "Ikke svart")
                    g.Allergies
                    (match g.WantsBus with | true -> "Ja" | false -> "Nei"))
            |> String.concat ","

        let requestData =
            sprintf
                "{\"channel\":\"#rsvp\",\"username\":\"sot\",\"text\":\"Ny påmelding:\",\"attachments\":[{\"mrkdwn_in\":[\"fields\"],\"fields\":[%s]}]}"
                guests


        let! result =
            Http.request url
            |> Http.method POST
            |> Http.content (BodyContent.Text requestData)
            |> Http.send


        if result.statusCode > 299 || result.statusCode < 200
        then failwith result.responseText

        return result.responseText
    }


let validate (model: Model) =
    model.Guests
    |> List.map (function
        | g when g.Name.Length < 2 -> Some "Husk å fylle inn navn på alle gjestene"
        | g when g.IsAttending.IsNone -> Some "Husk å krysse ja eller nei på alle gjestene"
        | _ -> Option.None)
    |> List.choose id
    |> List.tryHead


let isValid = validate >> Option.isNone


let update message model =

    match message with
    | SendClicked when isValid model ->
        { model with
              IsSending = true
              ValidationMessage = Option.None },
        Cmd.OfAsync.either downloadAsync model.Guests SubmitSuccess SubmitFailed
    | SendClicked ->
        { model with
              ValidationMessage = validate model },
        Cmd.none
    | NameChanged (id, value) -> updateGuest id (fun guest -> { guest with Name = value }) model, Cmd.none
    | AllergiesChanged (id, value) -> updateGuest id (fun guest -> { guest with Allergies = value }) model, Cmd.none
    | IsAttendingChanged (id, value) ->
        updateGuest id (fun guest -> { guest with IsAttending = Some value }) model, Cmd.none
    | WantBusChanged (id) ->
        updateGuest id (fun guest -> { guest with WantsBus = not guest.WantsBus }) model, Cmd.none    
    | AddGuestClicked ->
        { model with
              Guests = model.Guests @ [ newGuest () ] },
        Cmd.none
    | SubmitSuccess _ ->
        { model with
              IsSending = false
              Result = Ok() },
        Cmd.none
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
                    str "7.AUGUST 2021"//, FOLKVANG, DRØBAK"
                ]
            ]
        ]
        div [ css [ TextAlign.Center
                    MaxWidth'(rem 32.0)
                    Margin.Auto ] ] [
                div [] [
                    h3 [] [ str "Oppdatering (27. juni)" ]
                    p [] [
                        str "Det blir bryllup 7. august! Vi har bestemt oss for å flytte festen fra Folkvang til et sted hvor vi kan gjennomføre med gjeldende bestemmelser.
                        Detaljene er ikke fullstendig spikret, men vi føles oss trygge på at det blir feiring med mindre det blir gjeninnført strengere restriksjoner enn de som gjelder i dag.
                        Feiringen vil fremdeles finne sted i Oslo-området - vi kommer med bedre informasjon veldig snart!"
                        br []
                        br []
                    ]                   
                    divider "heart"
    
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
                        MaxWidth'(rem 36.0)
                        Margin.Auto ] ] [
                div [] [
                    divider "heart"
                    h3 [] [ str "Bryllupet" ]
                    p [] [
                        str "Festen avholdes 7.august 2021 i Oslo-området."
                        br []
                        str "Informasjon om hvor og når kommer veldig snart."
                    ]
                ]
                div [] [
                    divider "virus"
                    h3 [] [ str "Korona" ]
                    p [] [
                        str
                            "Vi vil gjennomføre i henhold til gjeldende forskrift. Nøyaktig informasjon om hva det innebærer kommer etter at vi har fått avtalt med arrangøren."                       
                        
                    ]
                ]
//                div [] [
//                    divider "home"
//                    h3 [] [ str "Overnatting" ]
//                    p [] [
//                        str "Det finnes overnatting både i Oslo og Drøbak - Folkvang er bare en halvtimes kjøretur fra Oslo og ligger fint til
//                        bussforbindelser. Velg det som passer deg best! Det blir også en egen buss fra Folkvang til
//                        Oslo sentrum ved festens slutt, kryss av i RSVP-skjemaet hvis man ønsker det."
//                    ]
//                ]
                div [] [
                    divider "envelope"
                    h3 [] [ str "RVSP" ]
                    p [] [
                        str
                            "Under kan dere svare på om dere kommer eller ikke - skriv eventuelle allergier eller andre behov som kommentar. Svar utbedes innen 15. juli."
                    ]
                    br []
                    br []
                    (match model.Result with
                     | Ok () ->
                         fragment [] [
                             h1 [ css [ Color' Styles.green ] ] [
                                 str "Takk! 💗"
                             ]
                             br []
                         ]
                     | Error message ->
                         div
                             []
                             ([ div [ css [ Display.Flex
                                            MarginBottom'(rem 0.5) ] ] [
                                 label [ (css
                                              (Styles.label
                                               @ [ Width'(pct 27)
                                                   MarginRight'(rem 1.0) ])) ] [
                                     str "Navn"
                                 ]
                                 label [ (css
                                              (Styles.label
                                               @ [ Width'(pct 27)
                                                   MarginRight'(rem 1.0) ])) ] [
                                     str "Kommentar"
                                 ]
                                 label [ (css (Styles.label @ [ Width'(pct 22) ])) ] [
                                     str "Kommer"
                                 ]
//                                 label [ (css (Styles.label @ [ Width'(pct 18) ])) ] [
//                                     str "Ønsker buss"
//                                 ]
                                ] ]
                              @ (model.Guests
                                 |> List.map (fun g ->
                                     div [ css [ Display.Flex
                                                 AlignItems.Center
                                                 TextAlign.Left
                                                 MarginBottom'(rem 0.5) ] ] [
                                         input [ css
                                                     (Styles.input
                                                      @ [ Width'(pct 27)
                                                          MarginRight'(rem 1.0) ])
                                                 Props.Type "text"
                                                 Value g.Name
                                                 OnChange
                                                 <| fun e -> dispatch <| NameChanged(g.Id, e.Value) ]

                                         input [ css
                                                     (Styles.input
                                                      @ [ Width'(pct 27)
                                                          MarginRight'(rem 1.0) ])
                                                 Props.Type "text"
                                                 Value g.Allergies
                                                 OnChange
                                                 <| fun e -> dispatch <| AllergiesChanged(g.Id, e.Value) ]
                                         div [ css [ Width'(pct 22) ] ] [
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
                                         div [ css [ Width'(pct 18) ] ] [
//                                             button [ css
//                                                      <| Styles.button
//                                                          Styles.blue
//                                                             (if g.WantsBus = true then
//                                                                 Styles.Primary
//                                                              else
//                                                                  Styles.Secondary)
//
//                                                      OnClick
//                                                      <| fun e -> dispatch <| WantBusChanged(g.Id) ] [
//                                                 icon "check"
//                                             ]
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
                                     |> Option.map (fun _ ->
                                         p [] [
                                             span [ css [ Color' Styles.pinkDark
                                                          FontWeight.Bold ] ] [
                                                 str "Noe gikk galt!"
                                             ]
                                             p [ css [ Color' Styles.black ] ] [
                                                 str
                                                     "Prøv igjen, prøv en annen nettleser eller bare send en god gammeldags melding 🙃"
                                             ]
                                             br []
                                         ])
                                     |> Option.defaultValue (fragment [] []))

                                    (model.ValidationMessage
                                     |> Option.map (fun msg ->
                                         div [ css [ Margin'(rem 1.0)
                                                     Color' Styles.pinkDark
                                                     FontWeight.Bold ] ] [
                                             str msg
                                         ])
                                     |> Option.defaultValue (fragment [] []))

                                    div [ css [ MarginTop'(rem 0.5) ] ] [
                                        button [ Disabled(model.IsSending)
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
                    divider "user-tie"
                    h3 [] [ str "Viktige personer" ]
                    p [] [
                        str "Toastmastere er "
                        a [ css Styles.link
                            Href "https://www.facebook.com/torbjornhar"
                            Target "_blank" ] [
                            str "Torbjørn"
                        ]
                        str " (torbjornhar at gmail.com) og "
                        a [ css Styles.link
                            Href "https://www.facebook.com/herman.l.hauge"
                            Target "_blank" ] [
                            str "Herman"
                        ]
                        str " (herman.l.hauge at gmail.com)."
                    ]
                    br []
                    br []
                    p [] [
                        str "Forlovere er "
                        a [ css Styles.link
                            Href "https://www.facebook.com/lydtekniker"
                            Target "_blank" ] [
                            str "Hans Arne"
                        ]
                        str ", "
                        a [ css Styles.link
                            Href "https://www.facebook.com/nicohvi"
                            Target "_blank" ] [
                            str "Nicolay"
                        ]
                        str " og "
                        a [ css Styles.link
                            Href "https://www.facebook.com/karoline.sorbo"
                            Target "_blank" ] [
                            str "Karoline"
                        ]
                        str "."
                    ]
                ]

                div [] [
                    divider "gift"
                    h3 [] [ str "Ønskeliste" ]
                    p [] [ str "Kommer senere!" ]
                ]
//                div [] [
//                    divider "question"
//                    h3 [] [ str "Spørsmål & Svar" ]
//                    p [] [ str "Kommer senere!" ]
//                ]
            ]
        ]
    ]

open Elmish.React

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
