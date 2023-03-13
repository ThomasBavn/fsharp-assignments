// GREEN

// 5.1

let sum (m: int) (n: int) =
    let rec summing (m: int) n count acc =
        match (count) with
        | (c) when c > n -> acc
        | (c) -> summing m n (c + 1) (acc + m + c)

    summing m n 0 0

sum 0 10 //55
sum 100 10 // 1155
sum 42 2345 // 2849217

// 5.2

let length lst =
    let rec addOne lst acc =
        match lst with
        | [] -> acc
        | _ :: xs -> addOne xs (acc + 1)

    addOne lst 0

// length [] // 0
// length [ 1; 2; 3; 4; 5 ] // 5
// length [ 0..1024 ] // 1025

// 5.3

let foldBack folder lst acc =
    let rec continuer folder l acc c =
        match l with
        | [] -> c acc
        | x :: xs -> continuer folder xs acc (fun r -> c (folder x r))

    continuer folder lst acc id //id = generic type function

// foldBack (-) [ 1..10 ] 0 // -5
// foldBack (-) [ 1..1000000 ] 0 //	-500000

// 5.4

let factC x =
    let rec continuer aux counter c =
        match counter with
        | 0 -> c aux
        | count -> continuer (aux * count) (count - 1) (fun r -> c r)

    continuer 1 x id

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)

    aux 1 x

let time f x = //taken from assignment 2
    let start = System.DateTime.Now
    let res = f x
    let finish = System.DateTime.Now
    (res, finish - start)

time factC 15 // 533 microseconds
time factA 15 // 476 microseconds

// YELLOW

// 5.5

let fibA x =
    let rec fibonacci x y acc =
        match x with
        | x when x < 1 -> acc
        | x -> fibonacci y (y - 1) (x + y) + fibonacci y (x - 1) (x + y)

    fibonacci x (x - 1) 0

fibA 10
