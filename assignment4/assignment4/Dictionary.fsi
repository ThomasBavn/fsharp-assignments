module Dictionary

// Courtesy to Albert Dyekjær for showing me the correct type signature
open System.Collections.Generic

type Dictionary =
    | Leaf of bool
    | Node of bool * Dictionary<char,Dictionary>

val empty: unit -> Dictionary
val insert : string -> Dictionary -> Dictionary
val lookup : string -> Dictionary -> bool
val step : char -> Dictionary -> (bool * Dictionary) option
