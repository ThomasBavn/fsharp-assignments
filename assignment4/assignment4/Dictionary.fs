module Dictionary

open System.Collections.Generic

type Dictionary =
    | Leaf of bool
    | Node of bool * Dictionary<char, Dictionary>

let empty (_: unit) = Leaf false

let splitup (str:string) = (str.[0],str.[1..])
let rec insert (str: string) (dictionary: Dictionary) =
    match (str, dictionary) with
    | "", Leaf _ -> Leaf true
    | "", Node (_, dict) -> Node(true, dict)
    | str, Leaf b ->
        let s, sx = splitup str
        let dict = Dictionary()
        dict.[s] <- insert sx (empty ()) //Courtesy to Lasse Faurby for showing how to insert into Dictionary at key
        Node(b, dict)
    | str, Node (b, dict) ->
        let s, sx = splitup str
        let exists, res = dict.TryGetValue s
        if exists then
            dict.[s] <- insert sx res
            Node(b, dict)
        else
            dict.[s] <- insert sx (empty ())
            Node(b, dict)

let rec lookup (str:string) (dictionary:Dictionary) =
    match (str,dictionary) with
    | "", Leaf _ -> true
    | "", Node(b,_) -> b
    | _, Leaf _ -> false
    | str, Node(_,dict) ->
        let s, sx = splitup str
        let exists, res = dict.TryGetValue s
        if exists then lookup sx res
        else false

let rec step c (dictionary:Dictionary) =
    match
