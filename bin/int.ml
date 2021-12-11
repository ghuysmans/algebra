module type S = sig
  include Algebra.Group.S
  val of_int : int -> t
  val to_int : t -> int
end

module Additive : S = struct
  type t = int
  let id = 0
  let equal = (=)
  let inv x = -x
  let (@) = (+)
  let of_int x = x
  let to_int x = x
end

let extended_euclid a b = (* taken from Wikipedia *)
  let rec f r u v r' u' v' =
    if r' = 0 then
      r, u, v
    else
      let q = r / r' in
      f r' u' v' (r - q * r') (u - q * u') (v - q * v')
  in
  f a 1 0 b 0 1

module Mod (N : sig val n : int end) : S = struct
  type t = int

  let id = 1
  let equal = (=)
  let (@) a b = (a * b) mod N.n
  let of_int n = n mod N.n
  let to_int n = n

  let inv x =
    let r, x', _ = extended_euclid x N.n in
    if r = 1 then
      if x' < 0 then x' + N.n else x'
    else
      raise (Invalid_argument "non-invertible value")
end
