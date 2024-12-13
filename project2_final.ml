(*Used this to be able to use regular expressions*)
#load "str.cma";;
(*Analyzes an input word (i.e. five letters followed by five numbers) and returns back a list of the characters
   not occuring in the potential answer words*)
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

(*For the 2s, this analyzes an input word and returns the letter of the character followed by a character representation of 
   its index in order to track where that character should occur in an answer word*)
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

(*For the 1s, this analyzes an input word and returns the letter of the character followed by a character representation of 
   its index in order to track where that character should not occur in an answer word*)
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

(*This function takes a word in the file being analyzed and tracks that the characters that should be included 
   are there and the ones that should not be inluded are not*)
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

(*This takes a word from the possible list and checks whether the characters that should occur actually occur and that they don't
   occur in the wrong spots*)
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

(*This checks a word in the possible word list and checks that the confirmed characters occur at the right spots*)
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

(*This checks an individual word from the testlist and checks if everything occurs in the right spots, and returns
   a list of possible answers*)
let rec checkWord sampleList zeroChars oneChars twoChars= 
  match sampleList with 
    | [] -> []
    | (head::rest) ->
      if compareNegatives head zeroChars oneChars twoChars && compareOnes head oneChars && compareTwos head twoChars then
        head:: checkWord rest zeroChars oneChars twoChars
      else
        checkWord rest zeroChars oneChars twoChars;;
    

  (*This generates the list of possible words from a file. I used ChatGPT to generate this part as we hadn't gone over it yet,
      and I wanted to get stuff working prior to the due date.It's pretty standard in terms of how things are done.*)
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
(*This builds the lists of potential words, looping through the input list, and calling the check word function on each
   word in the file. I used an append to ensure that I was making one list of strings, and not a list of lists.*)
let rec buildLists inputList words = 
  match inputList with
  |[] ->[]
  |(head::rest)->
    let zeroChars = analyzeNegative head in 
    let oneChars = analyzeOnes head in 
    let twoChars = analyzePositive head in
    let currentResult = checkWord words zeroChars oneChars twoChars in
    currentResult @ buildLists rest words;;

(*This creates a hashtable based off of the list of potential words, adding 1 to its associated value each time it 
   occurs. This is so I can print the words based off of the intersection of all words, like I did for the first
   project. I used ChatGPT a little bit here as I was having a syntax error as I was trying to return a list instead
   of the hash table by accident.*)
  let buildHashMap acceptableWords =
    let hashMap = Hashtbl.create (List.length acceptableWords) in
    let rec build hm = function
      | [] -> hm
      | h::t ->
          Hashtbl.replace hm h ((try Hashtbl.find hm h with Not_found -> 0) + 1);
          build hm t
    in
    build hashMap acceptableWords;;
(*This prints the words that occur for each input.
It prints the intersections of the guesses.
    I used ChatGPT to write it since it seemed like small potatoes*)
    let printWordsOfInputListSize hashMap inputListSize =
      Hashtbl.iter (fun key value ->
        if value = inputListSize then
          Printf.printf "%s\n" key
      ) hashMap;;
    

(*Essentially main. I handle input here, splitting into a list based on commas followed by spaces,
   and I call all the appropriate functions to get everything to run*)
let s = read_line () in
let inputList = Str.split (Str.regexp ", ") s in
let words = buildReferenceList "fives.txt" in
let acceptableWords = buildLists inputList words in
let myHash = buildHashMap acceptableWords in
printWordsOfInputListSize myHash (List.length inputList);;

(*
04/10/2024 Wordle
arise02000, front02201, proto02220
troth
broth
wroth

09/09/2023 Wordle
arise00000, could10110  
lunch
mulct
lucky
mulch
gulch

arise00000, could10110, lunch22010
lucky

09/16/2022 Wordle
arise11001, clear00112, pager22022
parer
payer
pawer
paver
pater
paper
*)









