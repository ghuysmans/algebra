open Algebra

let () =
  match Sys.argv with
  | [| _; s; n |] ->
    let n = int_of_string n in
    let module M = Str.Concat_counter in
    let module T = Monoid.Tools.S (M) in
    let t = T.fastpow (M.t_of_string s) ~b:n in
    Printf.printf "%s\n" (M.string_of_t t);
    Printf.eprintf "%d operations.\n" (M.count_of_t t)
  | [| _; b; e; m |] ->
    let b, e, m = int_of_string b, int_of_string e, int_of_string m in
    let module G_m = Int.Mod (struct let n = m end) in
    let module T = Monoid.Tools.S (G_m) in
    Printf.printf "%d\n" (G_m.to_int (T.fastpow (G_m.of_int b) ~b:e))
  | _ ->
    Printf.eprintf "usage: %s s rep | base exponent modulus\n" Sys.argv.(0);
    exit 1
