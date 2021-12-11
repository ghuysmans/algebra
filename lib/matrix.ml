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

  let of_list dir l =
    let a =
      let m, n =
        match dir with
        | `Column -> List.length l, 1
        | `Row -> 1, List.length l
      in
      zero m n
    in
    l |> List.iteri (fun i x ->
      match dir with
      | `Column -> a.(i).(0) <- x
      | `Row -> a.(0).(i) <- x
    );
    a
end
