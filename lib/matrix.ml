module type Element = sig
  type t
  val id_add : t
  val (+) : t -> t -> t
  val id_prod : t
  val ( * ) : t -> t -> t
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

  let id m n =
    if m <> n then raise (Invalid_argument "no identity for non-square matrices");
    let a = zero m n in
    for i = 0 to n - 1 do
      a.(i).(i) <- E.id_prod
    done;
    a

  let of_array dir inp =
    match dir with
    | `Column -> Array.(init (length inp) (fun i -> [| inp.(i) |]))
    | `Row -> [| Array.copy inp |]
end
