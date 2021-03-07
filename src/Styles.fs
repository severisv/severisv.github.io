module App.Styles

open Fss
let fontFancy = FontFamily.Custom "Lato"
let fontDisplay = FontFamily.Custom "Lato"
let fontText = FontFamily.Custom "Overlock"



let black = CssColor.Rgb(80, 80, 80)
let grey = CssColor.Hex "#f3f4ef"
let greyDark = CssColor.Hex "#D6DDCB"
let greenLight = CssColor.Hex "#c1d3c5"
let green = CssColor.Hex "#a1b59c"
let greenDark = CssColor.Hex "#758e67"
let yellow = CssColor.Hex "#fbf5dd"
let pink = CssColor.Hex "#B59CA3"
let pinkDark = CssColor.Hex "#9A7981"
let blue = CssColor.Hex "#6F95A5"
let blueLight = CssColor.Hex "#8AABB5"


let headers = [MarginTop' (rem 0.0)]
let h1 = headers @ [ FontSize'(rem 3.0); fontFancy ]
let h2 = headers @ [ FontSize'(rem 2.0) ]
let h3 = headers @ [ fontDisplay; FontSize'(rem 1.0); MarginBottom' (rem 0.5) ]


let borderRadius = BorderRadius' (px 4)

let body =
    [ Label' "body"
      fontText
      Color' black
      FontSize' (px 18)
      PaddingBottom' (rem 2.0)
      ]


let containerWidth = rem 50.0

let container =
    [ Label' "container"
      Margin.Auto
      MarginTop'(rem 0.0)
      MaxWidth' containerWidth
      PaddingLeft' (rem 1.0)
      PaddingRight' (rem 1.0)
       ]



let label = [
    Label' "label"
    TextAlign.Left
    TextTransform.Uppercase
    FontSize' (rem 0.8)
    FontWeight' (CssInt 600)
    LetterSpacing' (px 1)
]

let input = [
    Label' "input"    
    FontSize' (rem 1.0)
    fontText
    BorderColor' greyDark
    BorderWidth' (px 1)
    BorderStyle.Solid
    Padding' (rem 0.4)
    borderRadius
    BackgroundColor.white
    Outline.None
]


type BtnType = Primary | Secondary
let button color btnType = [
    Label' "button"    
    FontSize' (em 0.65)
    fontDisplay
    BorderColor' (color)
    BorderWidth' (px 1)
    MinWidth' (em 3.5)
    BorderStyle.Solid
    TextTransform.Uppercase
    PaddingLeft' (em 1.0)
    PaddingRight' (em 1.0)
    PaddingTop' (em 0.5)
    PaddingBottom' (em 0.5)
    borderRadius
    (match btnType with | Primary -> BackgroundColor' color | Secondary -> BackgroundColor.white)
    (match btnType with | Primary -> Color.white | Secondary -> Color' color)
    Outline.None
    Cursor.Pointer
    Hover [
        BackgroundColor' color
        Color.white
    ]
    TransitionProperty.BackgroundColor
    TransitionDuration' (sec 0.15)
    TransitionTimingFunction.EaseOut    
]

let link = [
    BackgroundColor.transparent
    Cursor.Pointer
    Border.None
    Outline.None
    Padding' (rem 0.0)
    fontDisplay
    Color' blue
    Hover [
        Color' blueLight
    ]
    TransitionProperty.Color
    TransitionDuration' (sec 0.15)
    TransitionTimingFunction.EaseOut
    TextDecoration.None
]