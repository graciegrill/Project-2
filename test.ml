#load "str.cma";;
let explode s = s|> String.to_seq |> List.of_seq;;
let rec length ls = 
  match ls with 
  |[]->0
  |(head::rest)-> (length rest) +1;;
let analyzeNegative inputWord =
  let rec track i accum =
    if i < 5 then
      if String.get inputWord (i + 5) = '0' then
        track(i + 1) (String.get inputWord i :: accum)
      else
        track(i + 1) accum
    else
      accum
  in
  track 0 [];;

let analyzePositive inputWord =
  let rec track i accum =
    if i < 5 then
      if String.get inputWord (i + 5) = '2' then
        track(i + 1) (String.get inputWord i ::Char.chr i:: accum)
      else
        track(i + 1) accum
    else
      accum
  in
  track 0 [];;

let analyzeOnes inputWord =
  let rec track i accum =
    if i < 5 then
      if String.get inputWord (i + 5) = '1' then
        track(i + 1) (String.get inputWord i ::Char.chr i:: accum)
      else
        track(i + 1) accum
    else
      accum
  in
  track 0 [];;

let rec compareNegatives word zeroChars oneChars twoChars =
  let rec loop remainingChars=
    match remainingChars with 
    |[]->true
    |(head::rest)->
        if (not(String.contains word head)) || List.mem head oneChars || List.mem head twoChars then
          loop rest
        else
          false
  in loop zeroChars;;

  let rec compareOnes word oneChars =
    let rec loop remainingChars index=
      match remainingChars with 
      |[]->true
      |[_]->false
      |(letter::position::rest)->
          if (index < List.length oneChars && String.contains word (List.nth oneChars (index))  && (String.get word (Char.code (List.nth oneChars (index+1)))) <> (List.nth oneChars (index))) then
            loop rest (index+2)
          else
            false
    in loop oneChars 0 ;;
  
  let rec compareTwos word twoChars =
    let rec loop remainingChars index=
      match remainingChars with 
      |[]->true
      |[_] -> false
      |(letter::position::rest)->
          if (index < List.length twoChars && (String.get word (Char.code (List.nth twoChars (index+1)))) = (List.nth twoChars (index))) then
            loop rest (index+2)
          else
            false
    in loop twoChars 0;;

let rec checkWord sampleList zeroChars oneChars twoChars= 
  match sampleList with 
    | [] -> []
    | (head::rest) ->
      if compareNegatives head zeroChars oneChars twoChars && compareOnes head oneChars && compareTwos head twoChars then
        head:: checkWord rest zeroChars oneChars twoChars
      else
        checkWord rest zeroChars oneChars twoChars;;
    

  (*I used ChatGPT to generate this part as we hadn't gone over it yet and I wanted to get stuff working prior to the due date.*)
    let buildReferenceList filename =
      let file = open_in filename in
      let rec read_words accum =
        try
          let line = input_line file in
          read_words (line :: accum)
        with
        | End_of_file -> accum
      in
      let words = read_words [] in
      close_in file;
      List.rev words;;

let rec buildLists inputList words = 
  match inputList with
  |[] ->[]
  |(head::rest)->
    let zeroChars = analyzeNegative head in 
    let oneChars = analyzeOnes head in 
    let twoChars = analyzePositive head in
    let currentResult = checkWord words zeroChars oneChars twoChars in
    currentResult @ buildLists rest words;;
  let buildHashMap acceptableWords =
    let hashMap = Hashtbl.create (List.length acceptableWords) in
    let rec build hm = function
      | [] -> hm
      | h::t ->
          Hashtbl.replace hm h ((try Hashtbl.find hm h with Not_found -> 0) + 1);
          build hm t
    in
    build hashMap acceptableWords;;

    let printWordsOfInputListSize hashMap inputListSize =
      Hashtbl.iter (fun key value ->
        if value = inputListSize then
          Printf.printf "%s\n" key
      ) hashMap;;
    

  (*let rec buildHashMap hashMap acceptableWords =
    let hashMap = Hashtbl.create in
      match acceptableWords with
        |[]->[]
        |(head::rest)->
          if (Hashtbl.mem head hashMap) then
            Hashtbl.replace hashMap head ((Hashtbl.find hashMap head)+1)
          else
            Hashtbl.add hashMap head 1
    in buildHashMap hashMap rest;;*)


let s = read_line () in
let inputList = Str.split (Str.regexp ", ") s in
let words = buildReferenceList "fives.txt" in
let acceptableWords = buildLists inputList words in
let myHash = buildHashMap acceptableWords in
printWordsOfInputListSize myHash (List.length inputList);;










