exception Break

(** Reads the text file [file] and returns a list of its lines in the form of a string list*)
let read_dict (file:string) : string list =
  let ic = open_in file in
  let l = ref [] in
  try (
    while true do
      let s = (input_line ic) in
      let s0 = String.sub s 0 (String.length s - 1) in
      l := s0 :: (!l)
    done;
    []
  ) with End_of_file -> close_in ic ;
  List.rev (!l)

(** Returns [true] whenever [c] is a letter of [w]*)
let in_center_letter (c:char) (word:string) : bool =
  try(
    for i=0 to (String.length word -1) do
      if word.[i]=c then raise Break
    done;
    false
  ) with _ -> true

(* Test *)
let () =
  assert (in_center_letter 'a' "baobab");
  assert (in_center_letter 'z' "zen");
  assert (in_center_letter 'e' "cake")

(** Returns [true] whenever [w] is written with only the letters of [letter_list]*)
let test_word (word:string) (letter_list:char list) : bool =
  try (
  for i=0 to (String.length word - 1) do
    if (not(List.mem word.[i] letter_list)) then
      (
        raise Break
      )
    else
      ()
  done;
  true)
with Break -> false

(* Test *)
let () =
  assert (test_word "bonjour" ['b';'o';'n';'j';'u';'r']);
  assert (test_word "bonjour" ['b';'o';'n';'u';'r'] = false);
  assert (test_word "bon" ['b';'o';'n';'u';'r']);
  assert (test_word "la" ['l';'a']);
  assert (test_word "bon" [] = false)

(** Takes a [word_list], a [letter_list] containing all allowed letters and the [center] letter,
    and returns a list containing all possible words*)
let rec filter_words (word_list:string list) (letter_list:char list) (center:char) : string list =
  match word_list with
  []->[]
  |w::q when (String.length w<4) -> filter_words q letter_list center
  |w::q when (not (in_center_letter center w)) -> filter_words q letter_list center
  |w::q -> if (test_word w letter_list) then (w::(filter_words q letter_list center))
          else filter_words q letter_list center

(* Test *)
let () =
  assert (filter_words ["bonjour";"bon";"bonbon";"hui"] ['b';'o';'n';'z'] 'b' = ["bonbon"])

(** Insertion in a list, without repetition*)
let rec insert x l = match l with
| [] -> [x]
| t::q when t=x -> l
| t::q -> t::(insert x q)

(* Test *)
let () =
  assert (insert 2 [1;6;8] = [1;6;8;2]);
  assert (insert 6 [1;6;8] = [1;6;8])

(** Computes the score of a given [word] and the current [bonus] letter*)
let score (word:string) (bonus:char) =
  let s = ref 0 in
  let l = ref [] in
  for i=0 to String.length word - 1 do
    l:= insert (word.[i]) (!l);
    if (word.[i])=bonus then s := (!s) + 5 + 45 (* Pour la stratégie*)
  done;
  if List.length (!l) = 7 then s := (!s) + 7;
  !s +
    match String.length word with
      | 4 -> 2
      | 5 -> 4
      | 6 -> 6
      | 7 -> 12
      | l -> 12 + 3*(l-7)

let score (word:string) (bonus:char) =
  let s = ref 0 in
  let l = ref [] in
  for i=0 to String.length word - 1 do
    l:= insert (word.[i]) (!l);
    if (word.[i])=bonus then s := (!s) + 5
  done;
  if List.length (!l) = 7 then s := (!s) + 7;
  !s +
    match String.length word with
      | 4 -> 2
      | 5 -> 4
      | 6 -> 6
      | 7 -> 12
      | l -> 12 + 3*(l-7)


(* The main program*)

(* The dictionnary*)
let word_list = read_dict "dict.txt"

let () =
  let letter_list = ref [] in
  if Array.length (Sys.argv) <> 8 then
    failwith "Expected 7 arguments";
  for i = 1 to 7 do
    if (String.length (Sys.argv.(i))>1) then
      failwith "Not letters"
    else
      letter_list := (Sys.argv.(i).[0])::(!letter_list)
  done;

  let center_letter = Sys.argv.(1).[0] in
  let filtered_list = ref (filter_words word_list (!letter_list) center_letter) in
  let counter = ref 1 in
  let game_score = ref 0 in

  while (!counter <= 12) do
    print_endline ("Type bonus letter for flower " ^ Int.to_string (!counter) ^": ");
    let user_input = In_channel.input_line In_channel.stdin in
    match user_input with
    | None -> ()
    | Some bonus_letter when String.length bonus_letter = 1 ->
      (
        let bonus = bonus_letter.[0] in
        let confirmed = ref false in
        while (not (!confirmed)) do
        filtered_list := List.sort (fun w1 w2 -> score w2 (bonus) - score w1 (bonus)) (!filtered_list);
        print_endline ("Suggested word: " ^ (String.uppercase_ascii (List.hd (!filtered_list))));
        print_endline "Confirm? (Y/n)";
          let user_input = In_channel.input_line In_channel.stdin in
          match user_input with
            |Some x when (String.length x = 0) || (x.[0]<>'n') ->
              (
                game_score := (!game_score) + score (List.hd (!filtered_list)) (bonus);
                filtered_list := List.tl (!filtered_list);
                incr counter;
                confirmed := true
              )
            |_ -> filtered_list := List.tl (!filtered_list)
        done
        )
        | Some bonus -> ()
        
    done;

  print_endline ("Score: " ^ (Int.to_string (!game_score)))