module type S = sig
  include Semigroup.S

  (** {id} is the (unique) identity element. *)
  val id: t
end

module type I = sig
  include Semigroup.I

  (** {make ()} creates an identity value. *)
  val make: unit -> t
end

module Direct_product = struct
  module S (A: S) (B: S) : S with type t = A.t * B.t = struct
    include Semigroup.Direct_product.S (A) (B)
    let id = A.id, B.id
  end

  module I (A: I) (B: I) : I with type t = A.t * B.t = struct
    include Semigroup.Direct_product.I (A) (B)
    let make () = A.make (), B.make ()
  end
end

module Tools = struct
  module S (M : S) = struct
    open M

    let fastpow a b =
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
        failwith "negative exponent"
      else
        f a b id

    let order t =
      let rec f u n =
        if equal u id then
          n
        else
          f (t @ u) (n + 1)
      in
      f t 0
  end

  module I (M : I) = struct
    open M

    let fastpow a b =
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
        failwith "negative exponent"
      else
        f a b (make ())

    let order t =
      let id = make () in
      let rec f u n =
        if equal u id then
          n
        else
          f (t @ u) (n + 1)
      in
      f t 0
  end
end
