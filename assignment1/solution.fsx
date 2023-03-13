//GREEN
// 1.1

let sqr x = x * x

// 1.2

let pow x n = System.Math.Pow(x, n)

// 1.3

let rec sum =
    function
    | 0 -> 0
    | x -> x + sum (x - 1)

// 1.4

let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | x -> fib (x - 1) + fib (x - 2)

fib 40
// 1.5

let dup str = str + str
dup "hello "

// 1.6

let rec dupn s n =
    match s, n with
    | _, 0 -> ""
    | s, n -> s + dupn s (n - 1)

// 1.7

let rec bin =
    function
    | _, 0 -> 1
    | n, k -> if k = n then 1 else bin (n - 1, k - 1) + bin (n - 1, k)

// YELLOW
// 1.8

let timeToMin (hh, mm) = hh * 60 + mm

let timediff t1 t2 = timeToMin t2 - timeToMin t1
timediff (12, 34) (13, 35)

// 1.9

let minutes t = timediff (00, 00) t

minutes (23, 1)

// 1.10

let curry func a b = func (a, b)

curry (fun (x, y) -> x + y) 5 3

let uncurry func (a, b) = func a b
uncurry (fun x y -> x + y) (5, 3)

//SCRABBLE

// 1.11


// empty should only be used to create the tile (char 0, 0) in practice.
// The reason why it has pos (renamed to _ since not used) is to make sure it has the Word signature of int -> (char * int)

type Word = int -> char * int
let empty (letter, value) : Word = fun _ -> (letter, value)
let theLetterA = empty ('A', 1)
theLetterA 0
theLetterA 42
theLetterA -762

// 1.12

(*

 *)
let add (newPos: int) (cv: char * int) word : Word =
    fun pos -> if pos = newPos then cv else word pos

let theLettersAB = add 1 ('B', 3) theLetterA
theLettersAB 0
theLettersAB 1
theLettersAB 42

// 1.13


let hello2 = empty (char 0, 0) |> add 0 ('H', 4)

let hello =
    empty (char 0, 0)
    |> add 0 ('H', 4)
    |> add 1 ('E', 1)
    |> add 2 ('L', 1)
    |> add 3 ('L', 1)
    |> add 4 ('O', 1)

hello 0
hello 1
hello 2
hello 3
hello 4
hello 5

// 1.14


let singleLetterScore (word: Word) pos = snd (word pos)
let doubleLetterScore (word: Word) pos = snd (word pos) * 2
let trippleLetterScore (word: Word) pos = snd (word pos) * 3

singleLetterScore hello 4
doubleLetterScore hello 4
trippleLetterScore hello 4
