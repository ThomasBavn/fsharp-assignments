// GREEN
// 3.1

type aExp =
    | N of int // Integer value
    | V of string // Variable
    | WL // Length of the word
    | PV of aExp // Point value of character at specific word index
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)

let rec arithEvalSimple =
    function
    | N n -> n
    | Add(a, b) ->
        let va = arithEvalSimple a
        let vb = arithEvalSimple b
        va + vb
    | Sub(a, b) ->
        let va = arithEvalSimple a
        let vb = arithEvalSimple b
        va - vb
    | Mul(a, b) ->
        let va = arithEvalSimple a
        let vb = arithEvalSimple b
        va * vb

let a1 = N 42
let a2 = N 4 .+. (N 5 .-. N 6)
let a3 = N 4 .*. N 2 .+. N 34
let a4 = (N 4 .+. N 2) .*. N 34
let a5 = N 4 .+. (N 2 .*. N 34)

// arithEvalSimple a1
// arithEvalSimple a2
// arithEvalSimple a3
// arithEvalSimple a4
// arithEvalSimple a5

// 3.2


let rec arithEvalState exp map =
    match exp with
    | N n -> n
    | V v ->
        Map.tryFind v map
        |> function
            | Some a -> a
            | None -> 0
    | Add(a, b) -> arithEvalState a map + arithEvalState b map
    | Sub(a, b) -> arithEvalState a map - arithEvalState b map
    | Mul(a, b) -> arithEvalState a map * arithEvalState b map

let a6 = V "x"
let a7 = N 4 .+. (V "y" .-. V "z")

arithEvalState a6 (Map.ofList [ ("x", 5) ])
arithEvalState a6 (Map.ofList [ ("y", 5) ])
arithEvalState a7 (Map.ofList [ ("x", 4); ("y", 5) ])
arithEvalState a7 (Map.ofList [ ("y", 4); ("z", 5) ])

// 3.3

type word = (char * int) list
let hello: word = [ ('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1) ]

let rec arithEval exp (word: word) state =
    match exp with
    | N n -> n
    | V v ->
        Map.tryFind v state
        |> function
            | Some a -> a
            | None -> 0
    | Add(a, b) -> arithEval a word state + arithEval b word state
    | Sub(a, b) -> arithEval a word state - arithEval b word state
    | Mul(a, b) -> arithEval a word state * arithEval b word state
    | WL -> word.Length
    | PV exp -> snd word.[arithEval exp word state]

let arithSingleLetterScore = PV(V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV(V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV(V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

// arithEval WL [] Map.empty
// arithEval WL hello Map.empty
// arithEval (PV(N 0)) hello Map.empty
// arithEval arithSingleLetterScore hello (Map.ofList [ ("_pos_", 4); ("_acc_", 0) ])
// arithEval arithSingleLetterScore hello (Map.ofList [ ("_pos_", 4); ("_acc_", 42) ])
// arithEval arithDoubleLetterScore hello (Map.ofList [ ("_pos_", 4); ("_acc_", 0) ])
// arithEval arithDoubleLetterScore hello (Map.ofList [ ("_pos_", 4); ("_acc_", 42) ])
// arithEval arithTripleLetterScore hello (Map.ofList [ ("_pos_", 4); ("_acc_", 0) ])
// arithEval arithTripleLetterScore hello (Map.ofList [ ("_pos_", 4); ("_acc_", 42) ])

// 3.4

type cExp =
    | C of char (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
    | CV of aExp (* Character lookup at word index *)

let rec charEval exp (word: word) state =
    match exp with
    | C c -> c
    | ToUpper exp -> System.Char.ToUpper(charEval exp word state)
    | ToLower exp -> System.Char.ToLower(charEval exp word state)
    | CV exp -> fst word.[arithEval exp word state]

// charEval (CV(V "x" .-. N 1)) hello (Map.ofList [ ("x", 5) ])

// 3.5

type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)

let (.<=.) a b =
    a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let rec boolEval exp (word: word) state =
    match exp with
    | TT -> true
    | FF -> false
    | AEq(a1, a2) -> arithEval a1 word state = arithEval a2 word state
    | ALt(a1, a2) -> arithEval a1 word state < arithEval a2 word state
    | Not b -> not (boolEval b word state)
    | Conj(b1, b2) -> boolEval b1 word state && boolEval b2 word state
    | IsDigit c -> System.Char.IsDigit(charEval c word state)
    | IsLetter c -> System.Char.IsLetter(charEval c word state)
    | IsVowel c -> "aeiouyAEIOUY".Contains(charEval c word state)

// boolEval TT [] Map.empty
// boolEval FF [] Map.empty
// boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [ ("x", 5); ("y", 7) ])
// boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [ ("x", 5); ("y", 7) ])

// YELLOW
// 3.6

let isConsonant c = ~~(IsVowel c)

// boolEval (isConsonant (C 'H')) [] Map.empty
// boolEval (isConsonant (C 'h')) [] Map.empty
// boolEval (isConsonant (C 'A')) [] Map.empty
// boolEval (isConsonant (CV(V "x"))) hello (Map.ofList [ ("x", 0) ])
// boolEval (isConsonant (CV(V "x"))) hello (Map.ofList [ ("x", 1) ])

// 3.7

type stmnt =
    | Skip (* does nothing *)
    | Ass of string * aExp (* variable assignment *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

let rec evalStmnt exp (word: word) (state: Map<string, int>) =
    match exp with
    | Skip -> state
    | Ass(k, v) -> state.Add(k, (arithEval v word state))
    | Seq(s1, s2) -> evalStmnt s1 word state |> evalStmnt s2 word
    | ITE(guard, stmnt1, stmnt2) ->
        if boolEval guard word state then
            evalStmnt stmnt1 word state
        else
            evalStmnt stmnt2 word state
    | While(guard, stmnt) ->
        match guard with
        | TT -> evalStmnt Seq (stmnt, evalStmnt (While(guard, evalStmnt stmnt word state)) word state) word state
        | FF -> state

evalStmnt (ITE(WL .>=. N 5, Ass("x", N 1), Ass("x", N 2))) hello Map.empty
evalStmnt (ITE(WL .<. N 5, Ass("x", N 1), Ass("x", N 2))) hello Map.empty
// evalStmnt (While(V "x" .<=. WL, Seq(Ass("y", V "y" .+. V "x"), Ass("x", V "x" .+. N 1)))) hello Map.empty

// evalStmnt
//     (While(V "x" .<=. WL, Seq(Ass("y", V "y" .+. V "x"), Ass("x", V "x" .+. N 1))))
//     hello
//     (Map.ofList [ ("x", 3); ("y", 100) ])
