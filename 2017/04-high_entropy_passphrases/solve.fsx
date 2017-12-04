// sets the current directory to be same as the script directory
System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)

open System

let split separators (x:string) = x.Split(separators)

// check whether the argument is a valid V1 passphrase 
// returns true if s doesn't contain the same word twice, false otherwise
let isValidPassphrase s =
  let duplicates = s |> split [|' '|] 
                     |> Seq.groupBy id
                     |> Seq.filter (fun (_, s) -> (Seq.length s) > 1)
  (Seq.length duplicates) = 0

// helper, taken from https://stackoverflow.com/questions/14667866/f-char-seq-strings
let toString : char seq -> string = Seq.map string >> String.concat ""

// normalize a word by sorting its characters in ascending order
let normalizeWord w =
  Seq.sort w
  |> toString

// check whether the argument is a valid V2 passphrase 
// returns true if s doesn't contain any words that are anagrams, false otherwise
let isValidPassphrase2 s =
  let words = s |> split [|' '|] 
  let normalized = words |> Seq.map normalizeWord

  let duplicates = normalized
                     |> Seq.groupBy id
                     |> Seq.filter (fun (_, s) -> (Seq.length s) > 1)
  (Seq.length duplicates) = 0

// poor man's assertion, since NUnit doesn't work out-of-the-box in Mono
let assertEquals expected actual msg =
  if expected = actual then
    "."
  else
    sprintf "ERROR in %s: expected %b, got %b\n" msg expected actual

// poor man's test helper
let testPassphrase input expected = 
  let check = isValidPassphrase(input)
  let s = assertEquals expected check input
  printfn "%s" s 

let testPassphrase2 input expected = 
  let check = isValidPassphrase2(input)
  let s = assertEquals expected check input
  printfn "%s" s 

// unit tests for part I
[ ("abc", true)
; ("abc abc", false)
; ("abc def abc", false)
; ("aa bb cc dd aaa", true)
]
|> List.iter (fun (input,expected) -> testPassphrase input expected)

// unit tests for part II
[ ("abcde xyz ecdab", false)
]
|> List.iter (fun (input, expected) -> testPassphrase2 input expected)


// LET'S SOLVE IT!

// solution for part I: 466
let input = System.IO.File.ReadLines("input.txt")
Seq.filter (fun s -> isValidPassphrase s) input
|> Seq.length
|> printfn "V1: %d"

// solution for part II: 251
// HACK - we should really re-use input, but this raises an error, since 
//      apparently, we cannot read from the same stream twice
let input2 = System.IO.File.ReadLines("input.txt")
Seq.filter (fun s -> isValidPassphrase2 s) input2
|> Seq.length
|> printfn "V2: %d"
