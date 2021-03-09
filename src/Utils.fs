module App.Utils

open System
open System.Text

let Z2dnZzI s = Encoding.UTF8.GetString(Convert.FromBase64String(s))
    