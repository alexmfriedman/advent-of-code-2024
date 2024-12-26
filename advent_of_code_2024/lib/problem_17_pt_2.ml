open! Core
open! Async

module Bit = struct
  type t =
    | Zero
    | One
  [@@deriving equal, enumerate, sexp_of, compare]

  let to_int = function
    | Zero -> 0
    | One -> 1
  ;;

  let of_int = function
    | 1 -> One
    | 0 -> Zero
    | _ -> failwith "unreachable"
  ;;
end

module Triple_bit = struct
  type t = Bit.t * Bit.t * Bit.t [@@deriving equal, enumerate]

  let to_int ((a, b, c) : t) =
    let a = Bit.to_int a in
    let b = Bit.to_int b in
    let c = Bit.to_int c in
    Int.shift_left a 2 + Int.shift_left b 1 + c
  ;;

  let of_int x : t =
    let a = Int.shift_right x 2 |> Bit.of_int in
    let b = Int.shift_right x 1 |> Int.bit_and 1 |> Bit.of_int in
    let c = Int.bit_and 1 x |> Bit.of_int in
    a, b, c
  ;;
end

module Bit_maybe_set = struct
  type t = Bit.t option [@@deriving sexp_of, compare]
end

module A_value = struct
  (* Representation of an integer, from least significant bit to most significant bit (flipped).
     Will have exactly 63 bits (if the solution is more then that, I give up) *)
  module T = struct
    type t = Bit_maybe_set.t array [@@deriving sexp_of, compare]
  end

  include T

  (* We only use this immutably *)
  let empty : t = Array.create ~len:63 None

  let add_fact_if_possible (t : t) ~index ~bit =
    match t.(index) with
    | Some bit' when Bit.equal bit bit' -> Some t
    | Some _bit' -> None
    | None ->
      let t' = Array.copy t in
      t'.(index) <- Some bit;
      Some t'
  ;;

  let add_fact_if_possible (t : t) ~index ~triple_bit:(a, b, c) =
    let open Option.Let_syntax in
    let%bind t = add_fact_if_possible t ~index ~bit:c in
    let%bind t = add_fact_if_possible t ~index:(index + 1) ~bit:b in
    add_fact_if_possible t ~index:(index + 2) ~bit:a
  ;;

  let to_min_int (t : t) : int =
    Array.mapi t ~f:(fun i maybe_bit ->
      let int_value =
        match maybe_bit with
        | Some Bit.Zero | None -> 0
        | Some One -> 1
      in
      Int.shift_left int_value i)
    |> Array.sum (module Int) ~f:Fn.id
  ;;

  include Comparable.Make_plain (T)
end

let program = [ 2; 4; 1; 3; 7; 5; 4; 2; 0; 3; 1; 5; 5; 5; 3; 0 ]

let run ~filename:_ =
  let a_value = A_value.empty in
  let possible_a_values =
    List.foldi
      program
      ~init:(A_value.Set.singleton a_value)
      ~f:(fun i possible_a_values next_int ->
        let possible_a_values' = ref A_value.Set.empty in
        List.iter Triple_bit.all ~f:(fun triple_bit ->
          let triple_bit_int = Triple_bit.to_int triple_bit in
          let right_side = Int.bit_xor 6 next_int |> Int.bit_xor triple_bit_int in
          let left_shift = Int.bit_xor triple_bit_int 3 in
          Set.iter possible_a_values ~f:(fun a_value ->
            Option.iter
              (A_value.add_fact_if_possible a_value ~index:(3 * i) ~triple_bit)
              ~f:(fun a_value ->
                Option.iter
                  (A_value.add_fact_if_possible
                     a_value
                     ~index:((3 * i) + left_shift)
                     ~triple_bit:(Triple_bit.of_int right_side))
                  ~f:(fun a_value ->
                    possible_a_values' := Set.add !possible_a_values' a_value))));
        !possible_a_values')
  in
  let min_value =
    Set.to_list possible_a_values
    |> List.map ~f:A_value.to_min_int
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in
  print_s ([%sexp_of: int * A_value.Set.t] (min_value, possible_a_values));
  Deferred.unit
;;
