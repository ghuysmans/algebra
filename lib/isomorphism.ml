module type S = sig
  type i
  type j
  val f : i -> j
  val f' : j -> i
end

module Reverse (S : S) = struct
  type i = S.j
  type j = S.i
  let f = S.f'
  let f' = S.f
end


module Char (I: sig val from: char end) = struct
  type i = char
  type j = int
  let f i = Char.code i - Char.code I.from
  let f' j = Char.chr (j + Char.code I.from)
end

module Named (E: sig type t val elements: t array end) = struct
  let h = Hashtbl.create (Array.length E.elements)
  let () = Array.iteri (fun i x -> Hashtbl.replace h x i) E.elements

  type i = E.t
  type j = int
  let f = Hashtbl.find h
  let f' = Array.get E.elements
end
