module Concatenative : Algebra.Monoid.S with type t = string = struct
  type t = string
  let id = ""
  let equal = (=)
  let (@) = (^)
end

module Concat_counter : sig
  include Algebra.Monoid.S
  val t_of_string : string -> t
  val string_of_t : t -> string
  val count_of_t : t -> int
end = struct
  type t = string * int
  let id = "", 0
  let (@) t t' =
    let s, c = t in
    let s', c' = t' in
    s ^ s', (* concatenate them normally *)
    (* ... but treat "squaring" differently! *)
    if t == t' then c + 1 else c + c' + 1
  let equal (s, _) (s', _) = s = s'
  let t_of_string s = s, 0
  let string_of_t (s, _) = s
  let count_of_t (_, c) = c
end
