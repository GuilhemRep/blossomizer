exception F

let read_dict (file:string) =
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
  List. rev (!l)

let rec in_char (c:char) l = match l with
  []->false
  |c'::q when c=c'->true
  |_::q -> in_char c q

let in_center_letter (c:char) w =
  try(
    for i=0 to (String.length w -1) do
      if w.[i]=c then raise F
    done;
    false
  ) with _ -> true

let test_word w letter_list =
  try (
  for i=0 to (String.length w - 1) do
    if (not(in_char w.[i] letter_list)) then
      (
        raise F
      )
    else
      ()
  done;
  true)
with F -> false

let () =
  assert (test_word "bonjour" ['b';'o';'n';'j';'u';'r']);
  assert (test_word "bonjour" ['b';'o';'n';'u';'r'] = false);
  assert (test_word "bon" ['b';'o';'n';'u';'r']);
  assert (test_word "la" ['l';'a']);
  assert (test_word "bon" [] = false)

let rec filter_words word_list letter_list center = match word_list with
  []->[]
  |w::q when (String.length w<4) -> filter_words q letter_list center
  |w::q when (not (in_center_letter center w)) -> filter_words q letter_list center
  |w::q -> if (test_word w letter_list) then (w::(filter_words q letter_list center))
          else filter_words q letter_list center

let () =
  assert (filter_words ["bonjour";"bon";"bonbon";"hui"] ['b';'o';'n';'z'] 'b' = ["bonbon"])

let rec print_list l = match l with
  | [] -> print_newline ()
  | t::q -> print_endline t ; print_list q

let rec insert x l = match l with
| [] -> [x]
| t::q when t=x -> l
| t::q -> t::(insert x q)

let score w bonus =
  let s = ref 0 in
  let l = ref [] in
  for i=0 to String.length w - 1 do
    l:= insert (w.[i]) (!l);
    if (w.[i])=bonus then s := (!s) + 5 + 45 (* Pour la stratégie*)
  done;
  if List.length (!l) = 7 then s := (!s) + 7;
  !s +
  match String.length w with
  | 4 -> 2
  | 5 -> 4
  | 6 -> 6
  | 7 -> 12
  | l -> 12 + 3*(l-7)


let () =
  let word_list = read_dict "dict1.txt" in
  let letter_list = ref [] in
  for i = 1 to 7 do
    if (String.length (Sys.argv.(i))>1) then
      raise F
    else
      letter_list := (Sys.argv.(i).[0])::(!letter_list)
  done;
  let center = Sys.argv.(1).[0] in
  let filtered_list = ref (filter_words word_list (!letter_list) center) in
  while true do
    let user_input = In_channel.input_line In_channel.stdin in
    match user_input with
    | None -> ()
    | Some bonus ->
      (
        filtered_list := List.sort (fun w1 w2 -> score w2 (bonus.[0]) - score w1 (bonus.[0])) (!filtered_list);
        print_endline (List.hd (!filtered_list));
        filtered_list := List.tl (!filtered_list)
      )
  done