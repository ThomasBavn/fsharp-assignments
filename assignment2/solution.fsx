// GREEN
// 2.1

let rec downto1 n =
    if n > 0 then [ n ] @ downto1 (n - 1) else []

let rec downto2 n =
    match n > 0 with
    | true -> [ n ] @ downto2 (n - 1)
    | false -> []

downto1 5
downto2 10
downto1 0
downto2 -42

// 2.2
let isEven (id, _) = id % 2 = 0

let rec removeOddIdx (list: 'a list) =
    list |> List.indexed |> List.filter isEven |> List.map (fun l -> snd l)

removeOddIdx ([]: int list)
removeOddIdx [ true ]

removeOddIdx
    [ "Marry"
      "had"
      "a"
      "little"
      "lamb"
      "its"
      "fleece"
      "was"
      "white"
      "as"
      "snow" ]

// 2.3

let rec combinePair list =
    match list with
    | x :: y :: xs -> [ (x, y) ] @ combinePair xs
    | [ _ ]
    | [] -> []


combinePair ([]: int list)
combinePair [ true; false ]

combinePair
    [ "Marry"
      "had"
      "a"
      "little"
      "lamb"
      "its"
      "fleece"
      "was"
      "white"
      "as"
      "snow" ]

// 2.4
// type complex = Complex
// type complex = (float * float)
type complex = { real: float; imaginary: float }

let mkComplex x y = { real = x; imaginary = y }

let complexToPair (c: complex) = (c.real, c.imaginary)

let (|+|) x y =
    { real = x.real + y.real
      imaginary = x.imaginary + y.imaginary }

let (|*|) x y =
    { real = x.real * y.real - x.imaginary * y.imaginary
      imaginary = x.imaginary * y.real + x.real * y.imaginary }

let (|-|) x y =
    x
    |+| { real = -y.real
          imaginary = -y.imaginary }

let (|/|) x y =
    let deno = (y.real ** 2.0 + y.imaginary ** 2.0)

    x
    |*| { real = y.real / deno
          imaginary = -y.imaginary / deno }

((mkComplex -3.3 10.3) |/| (mkComplex -3.2 -2.0)) |> complexToPair

// 2.5

let explode1 (s: string) = s.ToCharArray() |> List.ofArray

let rec explode2 (s: string) =
    match s with
    | "" -> []
    | s -> [ s.[0] ] @ explode2 (s.Remove(0, 1))

explode1 ""
explode1 "Hello World!"

explode2 ""
explode2 "Hello world!"

// 2.6

let helloArr = [ 'H'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!' ]

let implode (list: char list) =
    List.foldBack (fun c acc -> c.ToString() + acc) list ""

let implode2 (list: char list) =
    List.fold (fun acc c -> acc + c.ToString()) "" list

implode helloArr
implode []

implode2 helloArr
implode2 []

let implodeRev (list: char list) =
    List.fold (fun acc c -> c.ToString() + acc) "" list

let implodeRev2 (list: char list) =
    List.foldBack (fun c acc -> acc + c.ToString()) list ""

implodeRev helloArr
implodeRev []

implodeRev2 helloArr
implodeRev2 []

// 2.7

let toUpper s =
    s |> explode1 |> List.map System.Char.ToUpper |> implode

let toUpper2 = explode1 >> List.map System.Char.ToUpper >> implode

toUpper "Hello world!"
toUpper ""

toUpper2 "Hello world!"

// 2.8

let rec ack pair =
    match pair with
    | 0, n -> n + 1
    | m, 0 -> ack (m - 1, 1)
    | m, n -> ack (m - 1, ack (m, n - 1))

// YELLOW
// 2.9

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

time (fun () -> ack (3, 11))

let timeArg1 f a = time (fun () -> f a)

timeArg1 ack (3, 11)

// 2.10
// The solution is correct, but I don't understand how to use the function with fac

// let downto3 f n e = if n > 0 then f (n - 1) else e

let rec downto3 f n e =
    match (f, n, e) with
    | (_, _, e) when n <= 0 -> e
    | (f, n, e) -> downto3 f (n - 1) e |> f n

downto3 (fun _ x -> x * 2u) 32 1u - 1u

let fac n = downto3 (fun x acc -> x * acc) n 1

downto3 (fun _ x -> x * 5) 3 1

// let rec range g n =
//     match (g, n) with
//     | _ when n <= 0 -> []
//     | g, n -> range g (n - 1) @ [ downto3 g n  ]

// This solution was heavily inspired by the solution in this repo
// https://github.com/PatrickMatthiesen/FsAssignments/blob/master/FsAssignments/Assignment%202/Script1.fsx
// Please note that this is the only exercise where I used the solution from external sources.

// Use List.fold
let range g n =
    match (g, n) with
    | _ when n <= 0 -> []
    | g, n -> downto3 (fun x acc -> (g x) :: acc) n []


range fac 10
// range2 fac 6



// SEQUENCE TILES

type word = (char * int) list

// 2.11
let hello: word = [ ('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1) ]

// 2.12

type squareFun = word -> int -> int -> int

let singleLetterScore (w: word) pos acc = snd w.[pos] + acc
let doubleLetterScore (w: word) pos acc = 2 * snd w.[pos] + acc
let tripleLetterScore (w: word) pos acc = 3 * snd w.[pos] + acc

singleLetterScore hello 4 0
doubleLetterScore hello 4 0
tripleLetterScore hello 4 0

// 2.13

let doubleWordScore (_: word) (_: int) acc = acc * 2
let tripleWordScore (_: word) (_: int) acc = acc * 3

doubleWordScore hello 4 0
tripleWordScore hello 4 0
doubleWordScore hello 12345 42
tripleWordScore hello 12345 42

// RED

// 2.14

let isConsonant (c: char) = not ("aeiouyæøåAEIOUYLÆØÅ".Contains(c))

let oddConsonants (w: word) pos acc =
    w
    |> List.filter (fun w -> isConsonant (fst w))
    |> List.length
    |> fun l -> if l % 2 = 0 then acc else -acc


// Scrabble prep
type square = (int * squareFun) list

let SLS: square = [ (0, singleLetterScore) ]
let DLS: square = [ (0, doubleLetterScore) ]
let TLS: square = [ (0, tripleLetterScore) ]
let DWS: square = SLS @ [ (1, doubleWordScore) ]
let TWS: square = SLS @ [ (1, tripleWordScore) ]
