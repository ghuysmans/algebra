module type S = sig
  type i
  type j
  val f : i -> j
  val f' : j -> i
end


module Char (I: sig val from: char end) : S = struct
  type i = char
  type j = int
  let f i = Char.code i - Char.code I.from
  let f' j = Char.chr (j + Char.code I.from)
end

module Named (E: sig type t val elements: t array end) : S = struct
  let h = Hashtbl.create (Array.length E.elements)
  let () = Array.iteri (fun i x -> Hashtbl.replace h x i) E.elements

  type i = E.t
  type j = int
  let f = Hashtbl.find h
  let f' = Array.get E.elements
end
