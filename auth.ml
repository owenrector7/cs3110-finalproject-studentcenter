exception InvalidPassphrase of string
exception TooLargePassword

type password = string

(** https://csrc.nist.gov/csrc/media/publications/fips/180/4/archive/2012-03-06/documents/fips180-4.pdf
    is what Ben Mun based off of*)

(** initial hash values; defined by NIST *)
let h =
  [
    "0x6a09e667";
    "0xbb67ae85";
    "0x3c6ef372";
    "0xa54ff53a";
    "0x510e527f";
    "0x9b05688c";
    "0x1f83d9ab";
    "0x5be0cd19";
  ]

let constants =
  [
    "0x428a2f98";
    "0x71374491";
    "0xb5c0fbcf";
    "0xe9b5dba5";
    "0x3956c25b";
    "0x59f111f1";
    "0x923f82a4";
    "0xab1c5ed5";
    "0xd807aa98";
    "0x12835b01";
    "0x243185be";
    "0x550c7dc3";
    "0x72be5d74";
    "0x80deb1fe";
    "0x9bdc06a7";
    "0xc19bf174";
    "0xe49b69c1";
    "0xefbe4786";
    "0x0fc19dc6";
    "0x240ca1cc";
    "0x2de92c6f";
    "0x4a7484aa";
    "0x5cb0a9dc";
    "0x76f988da";
    "0x983e5152";
    "0xa831c66d";
    "0xb00327c8";
    "0xbf597fc7";
    "0xc6e00bf3";
    "0xd5a79147";
    "0x06ca6351";
    "0x14292967";
    "0x27b70a85";
    "0x2e1b2138";
    "0x4d2c6dfc";
    "0x53380d13";
    "0x650a7354";
    "0x766a0abb";
    "0x81c2c92e";
    "0x92722c85";
    "0xa2bfe8a1";
    "0xa81a664b";
    "0xc24b8b70";
    "0xc76c51a3";
    "0xd192e819";
    "0xd6990624";
    "0xf40e3585";
    "0x106aa070";
    "0x19a4c116";
    "0x1e376c08";
    "0x2748774c";
    "0x34b0bcb5";
    "0x391c0cb3";
    "0x4ed8aa4a";
    "0x5b9cca4f";
    "0x682e6ff3";
    "0x748f82ee";
    "0x78a5636f";
    "0x84c87814";
    "0x8cc70208";
    "0x90befffa";
    "0xa4506ceb";
    "0xbef9a3f7";
    "0xc67178f2";
  ]

type bit =
  | Zero
  | One

(** Debugging: Converts a binary to string with ", " as a separator *)
let string_bitl l =
  let rec aux = function
    | [] -> ""
    | [ h ] -> begin
        match h with
        | Zero -> "0"
        | One -> "1"
      end
    | h :: t ->
        begin
          begin
            match h with
            | Zero -> "0"
            | One -> "1"
          end
          ^ ", " ^ aux t
        end
  in
  "(" ^ aux l ^ ")"

(** Debugging: Converts a binary to string without separator*)
let str_bit_no_sep l =
  let rec aux = function
    | [] -> ""
    | [ h ] -> begin
        match h with
        | Zero -> "0"
        | One -> "1"
      end
    | h :: t ->
        begin
          begin
            match h with
            | Zero -> "0"
            | One -> "1"
          end
          ^ aux t
        end
  in
  aux l

(** Debugging: String of binaries in 32-bit word blocks*)
let string_bitll lst =
  "[" ^ List.fold_left (fun acc x -> acc ^ string_bitl x ^ "; ") "" lst ^ "]"

(** Debugging: String of binaries in 32-bit word blocks and 512-bit message
    blocks*)
let string_bitlll lst =
  List.fold_left (fun acc x -> acc ^ string_bitll x ^ "| ") "" lst

(** a^b *)
let rec expo a b = if b = 0 then 1 else a * expo a (b - 1)

(** Converts a binary to int *)
let int_of_bitlist lt =
  let rec helper lst =
    match lst with
    | [] -> 0
    | h :: t -> begin
        match h with
        | Zero -> helper t
        | One -> expo 2 (List.length lst - 1) + helper t
      end
  in
  helper lt

(** Converts an integer to its binary equivalent. *)
let bitlist_of_int i =
  let rec helper current_int lst =
    if current_int = 0 then lst
    else
      helper (current_int / 2)
        ((if current_int mod 2 = 1 then One else Zero) :: lst)
  in
  helper i []

(** pads a binary with [n] 0's to the left*)
let rec pad_0_left n lst =
  if n = 0 then lst else pad_0_left (n - 1) (Zero :: lst)

(** pads a binary with [n] 0's to the right*)
let rec pad_0_right n lst =
  if n = 0 then lst else pad_0_right (n - 1) (lst @ [ Zero ])

(** Adds two binaries. Note that the resulting binary may not be 32-bit, so pad
    if necessary. *)
let add a b =
  let m_int = expo 2 32 in
  (int_of_bitlist a + int_of_bitlist b) mod m_int |> bitlist_of_int

(** Converts plaintext to binary. The first 8 indices the list store the 8-bits
    of the plaintext's first character.*)
let bitlist_of_plaintext plaintext =
  (* Converts plaintext to decimal integers. The head of the list stores the
     decimal representation of the plaintext's first character.*)
  let rec helper pos lst =
    if pos = -1 then lst
    else helper (pos - 1) (String.get_uint8 plaintext pos :: lst)
  in
  let intermediate_intlist = helper (String.length plaintext - 1) [] in
  let final =
    List.map
      (fun x ->
        let preprocess = bitlist_of_int x in
        let l = List.length preprocess in
        if l > 8 then failwith "This should not happen 3"
        else pad_0_left (8 - l) preprocess)
      intermediate_intlist
    |> List.flatten
  in
  final

(** Padding of binary that is converted from plaintext *)
let padding_sha256 pre_mature =
  let l = List.length pre_mature in
  let n = if l <= 447 then 447 - l else 512 - ((l - 447) mod 512) in
  let x = pad_0_right n (pre_mature @ [ One ]) in
  let l_in_bits = bitlist_of_int l in
  let final = x @ pad_0_left (64 - List.length l_in_bits) l_in_bits in
  final

(** Drops first n bits*)
let rec drop n lst1 = if n = 0 then lst1 else drop (n - 1) (List.tl lst1)

(** Takes first n bits *)
let rec take n lst2 =
  if n = 0 then [] else List.hd lst2 :: take (n - 1) (List.tl lst2)

(** lst[from .. until]*)
let slice (from, until) lst =
  if from > until then raise (failwith "This should not happen 2")
  else drop from lst |> take (until - from + 1)

(** The innermost list represents a 32-bit word block and the next outer layer
    of list represents the 512-bit message blocks*)
let parsing (message : bit list) =
  if List.length message mod 512 != 0 then failwith "This should not happen 1"
  else
    let num = List.length message / 512 in
    let rec helper_mblocks n lst =
      if n = 0 then lst
      else
        helper_mblocks (n - 1)
          (slice (512 * (n - 1), (512 * n) - 1) message :: lst)
    in
    let message_blocks = helper_mblocks num [] in
    let rec helper_wblocks n lst reference =
      if n = 0 then lst
      else
        helper_wblocks (n - 1)
          (slice (32 * (n - 1), (32 * n) - 1) reference :: lst)
          reference
    in
    let final = List.map (fun x -> helper_wblocks 16 [] x) message_blocks in
    if
      List.for_all
        (fun x ->
          List.length x = 16 && List.for_all (fun y -> List.length y = 32) x)
        final
    then final
    else failwith "This should not happen 4"

(** Bitwise logical and*)
let band lst1 lst2 =
  let final =
    List.map2
      (fun x y ->
        match (x, y) with
        | One, One -> One
        | _ -> Zero)
      lst1 lst2
  in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

(** Bitwise logical or*)
let bor lst1 lst2 =
  let final =
    List.map2
      (fun x y ->
        match (x, y) with
        | Zero, Zero -> Zero
        | _ -> One)
      lst1 lst2
  in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

(** Bitwise logical xor*)
let bxor lst1 lst2 =
  let final =
    List.map2
      (fun x y ->
        match (x, y) with
        | Zero, One | One, Zero -> One
        | _ -> Zero)
      lst1 lst2
  in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

(** Bitwise logical not*)
let bnot lst1 =
  let final =
    List.map
      (fun x ->
        match x with
        | Zero -> One
        | One -> Zero)
      lst1
  in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

(** Bitwise Leftshift*)
let ( << ) x n =
  let final =
    if n > List.length x then
      raise (Failure "you cannot drop more than the lenggth of the list ")
    else drop n x |> pad_0_right n
  in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

(** Bitwise Rightshift*)
let ( >> ) x n =
  let final = take (List.length x - n) x |> pad_0_left n in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

(** Rotate left *)
let rotl n x =
  let final = bor (x << n) (x >> List.length x - n) in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

(** Rotate Right *)
let rotr n x =
  let final = bor (x >> n) (x << List.length x - n) in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

let ch x y z =
  let final = bxor (band x y) (band (bnot x) z) in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

let maj x y z =
  let final = bxor (bxor (band x y) (band x z)) (band y z) in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

let cap_sigma0 x =
  let final = bxor (bxor (rotr 2 x) (rotr 13 x)) (rotr 22 x) in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

let cap_sigma1 x =
  let final = bxor (bxor (rotr 6 x) (rotr 11 x)) (rotr 25 x) in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

let low_sigma0 x =
  let s1 = rotr 7 x in
  let s2 = rotr 18 x in
  let final = bxor s1 s2 >> 3 in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

let low_sigma1 x =
  let s1 = rotr 17 x in
  let s2 = rotr 19 x in
  let final = bxor s1 s2 >> 10 in
  if List.length final != 32 then pad_0_left (32 - List.length final) final
  else final

(** Initial hash values in binary*)
let h_in_bits =
  List.map
    (fun x ->
      let pre_pad = int_of_string x |> bitlist_of_int in
      pad_0_left (32 - List.length pre_pad) pre_pad)
    h

(** Constants in binary *)
let k_in_bits =
  List.map
    (fun x ->
      let pre_pad = int_of_string x |> bitlist_of_int in
      pad_0_left (32 - List.length pre_pad) pre_pad)
    constants

(** Sets the message schedule given a message block*)
let set_message_schedule m =
  (* Helper of scheduler function *)
  let rec h_schedule i working_list =
    if i = 64 then working_list
    else
      let pre_pad =
        add
          (add
             (low_sigma1 (List.nth working_list (i - 2)))
             (List.nth working_list (i - 7)))
          (add
             (low_sigma0 (List.nth working_list (i - 15)))
             (List.nth working_list (i - 16)))
      in
      let wt = pad_0_left (32 - List.length pre_pad) pre_pad in
      if List.length wt != 32 then raise (Failure "wt not 32-bit")
      else h_schedule (i + 1) (working_list @ [ wt ])
  in
  let final = h_schedule 16 m in
  if
    List.length final != 64 || List.for_all (fun x -> List.length x != 32) final
  then raise (Failure "schedule invariant violated")
  else final

(** Implements step3 in the NIST SHA 256 specification in FIPS publication *)
let rec step3 t intermediate_vars schedule =
  if t = 64 then intermediate_vars
  else if
    List.for_all (fun x -> List.length x != 32) intermediate_vars
    || List.length schedule != 64
    || List.for_all (fun x -> List.length x != 32) schedule
  then raise (Failure "step 3 invariant violated")
  else
    let a = List.nth intermediate_vars 0 in
    let b = List.nth intermediate_vars 1 in
    let c = List.nth intermediate_vars 2 in
    let d = List.nth intermediate_vars 3 in
    let e = List.nth intermediate_vars 4 in
    let f = List.nth intermediate_vars 5 in
    let g = List.nth intermediate_vars 6 in
    let h = List.nth intermediate_vars 7 in
    let t1 =
      h
      |> add (cap_sigma1 e)
      |> add (ch e f g)
      |> add (List.nth k_in_bits t)
      |> add (List.nth schedule t)
    in
    let t2 = cap_sigma0 a |> add (maj a b c) in
    let t1_p_t2 = add t1 t2 in
    let d_p_t1 = add d t1 in
    let result =
      [
        pad_0_left (32 - List.length t1_p_t2) t1_p_t2;
        a;
        b;
        c;
        pad_0_left (32 - List.length d_p_t1) d_p_t1;
        e;
        f;
        g;
      ]
    in
    step3 (t + 1) result schedule

(** Helper of the hashing function. Note that there is a lot of asserts going on
    because it was quite painful to debug this pesky function. *)
let rec h_hashing = function
  | [] -> h_in_bits
  | h :: t ->
      let schedule = set_message_schedule h in
      let prev_hashval = h_hashing t in
      if
        List.length prev_hashval != 8
        || List.for_all (fun x -> List.length x != 32) prev_hashval
      then raise (Failure "prev_hashval invariant violated")
      else
        let final_vars = step3 0 prev_hashval schedule in
        if
          List.length final_vars != 8
          || List.for_all (fun x -> List.length x != 32) final_vars
        then raise (Failure "prev_hashval invariant violated")
        else
          let final =
            List.map2
              (fun x y ->
                let l = add x y in
                pad_0_left (32 - List.length l) l)
              final_vars prev_hashval
          in
          if
            List.length final != 8
            || List.for_all (fun x -> List.length x != 32) final
          then raise (Failure "final hash value violated")
          else final

(** Given the parsed 512-bit blocks of info, this function returns the final
    hash values*)
let hashing msg =
  let final = h_hashing (List.rev msg) in
  final

(** Converts an decimal to hexadecimal*)
let hexstring_of_int i =
  let rec hexstring_helper acc i =
    if i = 0 then acc
    else
      hexstring_helper
        ((let s = i mod 16 in
          if 0 <= s && s <= 9 then string_of_int s
          else if s = 10 then "a"
          else if s = 11 then "b"
          else if s = 12 then "c"
          else if s = 13 then "d"
          else if s = 14 then "e"
          else "f")
        ^ acc)
        (i / 16)
  in
  if i = 0 then "0" else hexstring_helper "" i

(** Converts a binary to hexadecimal *)
let rec hexstring_of_binary lst =
  if List.length lst mod 4 != 0 then
    raise (Failure "Invalid arg for hexstring_binary")
  else
    match lst with
    | [] -> ""
    | a :: b :: c :: d :: t ->
        let h = [ a; b; c; d ] |> int_of_bitlist |> hexstring_of_int in
        if String.length h != 1 then
          failwith "something is wrong in hexstring_binary"
        else h ^ hexstring_of_binary t
    | _ -> failwith "This should be impossible if List.length lst mod 4 = 0"

(** Given a string, returns a hexadecimal digest*)
let hash msg =
  msg |> bitlist_of_plaintext |> padding_sha256 |> parsing |> hashing
  |> List.flatten |> hexstring_of_binary