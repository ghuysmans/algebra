module type Element = sig
  type t
  val id_add : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val id_prod : t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val abs : t -> float (* FIXME *)
end

module type S = sig
  type element
  include Semigroup.S with type t = element array array
  val zero : int -> int -> t
  val id : int -> t
  val of_array : [< `Column | `Row] -> element array -> t
end

module type Invertible = sig
  include S
  val inv : t -> t
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

  (* almost https://fr.wikipedia.org/wiki/%C3%89limination_de_Gauss-Jordan *)
  let inv a =
    let a = Array.(map copy) a in
    let n = Array.length a in
    let inv = id n in
    let div ~by i =
      let f a =
        for j = 0 to n - 1 do
          a.(i).(j) <- E.(a.(i).(j) / by)
        done
      in
      f a;
      f inv
    in
    let swap i i' =
      let f a =
        for j = 0 to n - 1 do
          let tmp = a.(i).(j) in
          a.(i).(j) <- a.(i').(j);
          a.(i').(j) <- tmp
        done
      in
      f a;
      f inv
    in
    let sub i i' k =
      let f a =
        for j = 0 to n - 1 do
          a.(i).(j) <- E.(a.(i).(j) - k * a.(i').(j))
        done
      in
      f a;
      f inv
    in
    let r = ref 0 in
    for j = 0 to n - 1 do
      let k = ref !r in
      for i = !r to n - 1 do
        if E.abs a.(i).(j) > E.abs a.(!k).(j) then k := i
      done;
      div ~by:a.(!k).(j) !k;
      if !k <> !r then swap !k !r;
      for i = 0 to n - 1 do
        if i <> !r then sub i !r a.(i).(j)
      done;
      incr r
    done;
    inv
end

module I = Make (struct
  type t = int
  let id_add = 0
  let (+) = (+)
  let (-) = (-)
  let id_prod = 1
  let ( * ) = ( * )
  let (/) = (/)
  let abs x = float (abs x)
end)

module F = Make (struct
  type t = float
  let id_add = 0.
  let (+) = (+.)
  let (-) = (-.)
  let id_prod = 1.
  let ( * ) = ( *. )
  let ( / ) = ( /. )
  let abs = abs_float
end)

module C = Make (struct
  open Complex
  type nonrec t = t
  let id_add = zero
  let (+) = add
  let (-) = sub
  let id_prod = one
  let ( * ) = mul
  let (/) = div
  let abs = norm
end)
