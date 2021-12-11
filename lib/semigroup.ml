module type S = sig
  type t
  val equal: t -> t -> bool

  (** {@} must be associative. *)
  val (@): t -> t -> t
end

(** Imperative signature *)
module type I = sig
  include S

  val copy: t -> t
end

module Direct_product = struct
  module S (A: S) (B: S) : S with type t = A.t * B.t = struct
    type t = A.t * B.t
    let equal (a, b) (a', b') = A.equal a a' && B.equal b b'
    let (@) (a, b) (a', b') = A.(a @ a'), B.(b @ b')
  end

  module I (A: I) (B: I) : I with type t = A.t * B.t = struct
    include S (A) (B)
    let copy (a, b) = A.copy a, B.copy b
  end
end

module Tools = struct
  module S (M : S) = struct
    open M

    let fastpow a ~b =
      let rec f a b r =
        if b = 0 then
          (* a^0 * r = 1 * r = r *)
          r
        else if b (* is even *) land 1 = 0 then
          (* a^(2k) = (a^2)^k *)
          f (a @ a) (b / 2) r
        else
          (* a^(2k+1) = a * a^(2k) = a * (a^2)^c *)
          f (a @ a) (b / 2) (r @ a)
      in
      if b < 0 then
        raise (Invalid_argument "negative exponent")
      else if b = 0 then
        raise (Invalid_argument "unknown identity")
      else
        f a (b - 1) a

    let order t =
      let rec f u n =
        if equal u t then
          n - 1
        else
          f (t @ u) (n + 1)
      in
      f t 0
  end

  module I (M : I) = struct
    open M

    let fastpow a ~b =
      let rec f a b r =
        if b = 0 then
          (* a^0 * r = 1 * r = r *)
          r
        else if b (* is even *) land 1 = 0 then
          (* a^(2k) = (a^2)^k *)
          f (a @ a) (b / 2) r
        else
          (* a^(2k+1) = a * a^(2k) = a * (a^2)^c *)
          f (a @ a) (b / 2) (r @ a)
      in
      if b < 0 then
        raise (Invalid_argument "negative exponent")
      else if b = 0 then
        raise (Invalid_argument "unknown identity")
      else
        f a (b - 1) a

    let order t =
      let rec f u n =
        if equal u t then
          n - 1
        else
          f (t @ u) (n + 1)
      in
      f t 0
  end
end
