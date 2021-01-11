module App

type Model =
    {
        downloaded: Result<string, string>
    }
    with
        static member createDefault () = { downloaded = Ok "Ikke noe enda" }

type Msg =
    | OnDownloadClicked
    | ResetClicked
    | OnDownloadedSuccess of string
    | OnDownloadedFail of exn

open Fable.Core

[<Emit("Math.random()")>]
let getRandom(): float = jsNative
    
let downloadAsync path = async {
    do! Async.Sleep(1000) // emulate work
    
    let networkEmulated =
        if (getRandom () * 100.) > 50. then
            "Yay! Snart!"
        else
            raise (System.Exception("Ikke heeelt enda!"))
            
    return networkEmulated
}    
    
open Elmish

let init () =
    Model.createDefault (), Cmd.none
    
let update message model =
    match message with
    | OnDownloadClicked ->
        model, Cmd.OfAsync.either downloadAsync "/randomData" OnDownloadedSuccess OnDownloadedFail
    | OnDownloadedSuccess data ->
        { model with downloaded = Ok data }, Cmd.none
    | OnDownloadedFail ex ->
        { model with downloaded = Error ex.Message }, Cmd.none
    | ResetClicked ->
        Model.createDefault (), Cmd.none

open Fable.React
open Fable.React.Props
        
let view (model: Model) dispatch =
   let resultView =
       match model.downloaded with
       | Ok r ->
           h3 [ Style [CSSProp.Color "blue"] ] [ str r ]
       | Error er ->
           h3 [ Style [CSSProp.Color "red"] ] [ str er ]
   
   div [] [
       h1 [] [ str "Herro!" ]
       button [ OnClick (fun _ -> dispatch OnDownloadClicked) ] [ str "Prøv lykken" ]
       button [ OnClick (fun _ -> dispatch ResetClicked) ] [ str "Reset" ]
       div [] [ h2 [] [ str "Skjer?" ]; resultView ]
   ]
   
open Elmish.React
        
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run