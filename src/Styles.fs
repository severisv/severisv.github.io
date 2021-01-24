module App.Styles

open Fss

let fontFancy = FontFamily.Custom "Lato"
let fontDisplay = FontFamily.Custom "Lato"
let fontText = FontFamily.Custom "Overlock"



let black = CssColor.Rgb(80, 80, 80)
let grey = CssColor.Hex "#f3f4ef"
let greenLight = CssColor.Hex "#c1d3c5"
let green = CssColor.Hex "#a1b59c"
let greenDark = CssColor.Hex "#758e67"
let yellow = CssColor.Hex "#fbf5dd"


let headers = [MarginTop' (rem 0.0)]
let h1 = headers @ [ FontSize'(rem 3.0); fontFancy ]
let h2 = headers @ [ FontSize'(rem 2.0) ]
let h3 = headers @ [ fontDisplay; FontSize'(rem 1.0); MarginBottom' (rem 0.5) ]


let body =
    [ Label' "body"
      fontText
      Color' black
      FontSize' (px 18)
      PaddingBottom' (rem 2.0)]


let containerWidth = rem 48.0

let container =
    [ Label' "container"
      Margin.Auto
      MarginTop'(rem 0.0)
      MaxWidth' containerWidth ]
