// sets the current directory to be same as the script directory
System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)

open System

let split separators (x:string) = x.Split(separators)

let checkPassphrase s =
  //let words = split [|' '|] s
  let duplicates = s |> split [|' '|] 
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
  let check = checkPassphrase(input)
  let s = assertEquals expected check input
  printfn "%s" s 

// unit tests
[ ("abc", true)
; ("abc abc", false)
]
|> List.iter (fun (input,expected) -> testPassphrase input expected)
