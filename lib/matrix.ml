module type Element = sig
  type t
  val id_add : t
  val (+) : t -> t -> t
  val id_prod : t
  val ( * ) : t -> t -> t
end

module type S = sig
  type element
  include Semigroup.S with type t = element array array
  val zero : int -> int -> t
  val id : int -> t
  val of_array : [< `Column | `Row] -> element array -> t
end

module Make (E : Element) = struct
  type t = E.t array array

  let equal = (=)

  let (@) a b =
    let m, n = Array.length a, Array.length a.(0) in
    let m', n' = Array.length b, Array.length b.(0) in
    if n <> m' then raise (Invalid_argument "inconsistent sizes");
    Array.init m (fun i ->
      Array.init n' (fun j ->
        let acc = ref E.id_add in
        for k = 0 to n - 1 do
          acc := E.(!acc + a.(i).(k) * b.(k).(j))
        done;
        !acc
      )
    )

  let zero m n =
    Array.make_matrix m n E.id_add

  let id n =
    let a = zero n n in
    for i = 0 to n - 1 do
      a.(i).(i) <- E.id_prod
    done;
    a

  let of_array dir inp =
    match dir with
    | `Column -> Array.(init (length inp) (fun i -> [| inp.(i) |]))
    | `Row -> [| Array.copy inp |]
end

module I = Make (struct
  type t = int
  let id_add = 0
  let (+) = (+)
  let id_prod = 1
  let ( * ) = ( * )
end)

module F = Make (struct
  type t = float
  let id_add = 0.
  let (+) = (+.)
  let id_prod = 1.
  let ( * ) = ( *. )
end)

module C = Make (struct
  open Complex
  type nonrec t = t
  let id_add = zero
  let (+) = add
  let id_prod = one
  let ( * ) = mul
end)
