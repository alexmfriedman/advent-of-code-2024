open! Core
open! Async

module Secret_number = struct
  type t = int

  let mix (t : t) k = Int.bit_xor t k
  let prune (t : t) = t % 16777216

  let succ t =
    let t = mix t (t * 64) |> prune in
    let t = mix t (t / 32) |> prune in
    let t = mix t (t * 2048) |> prune in
    t
  ;;
end

module Price_sequence = struct
  module T = struct
    type t =
      { a : int
      ; b : int
      ; c : int
      ; d : int
      }
    [@@deriving sexp_of, equal, compare, hash]
  end

  include T
  include Hashable.Make_plain (T)

  let next { a = _; b; c; d } e = { a = b; b = c; c = d; d = e }
end

let run ~filename =
  let%map secret_numbers = Reader.file_lines filename >>| List.map ~f:Int.of_string in
  let seq_of_changes =
    Sequence.unfold ~f:(fun pred ->
      let succ = Secret_number.succ pred in
      Some (((succ % 10) - (pred % 10), succ % 10), succ))
  in
  let by_price_sequence = Price_sequence.Table.create () in
  List.iter secret_numbers ~f:(fun init ->
    let visited_price_sequences = Price_sequence.Hash_set.create () in
    let seq_of_changes = seq_of_changes ~init in
    let initial_price_sequence =
      match Sequence.take seq_of_changes 3 |> Sequence.to_list |> List.map ~f:fst with
      | [ b; c; d ] -> { Price_sequence.a = -1; b; c; d }
      | _ -> failwith "unreachable"
    in
    Sequence.drop seq_of_changes 3
    |> Fn.flip Sequence.take 1996
    |> Sequence.fold
         ~init:initial_price_sequence
         ~f:(fun prev_price_sequence (curr_change, curr_price) ->
           let curr_price_sequence =
             Price_sequence.next prev_price_sequence curr_change
           in
           if not (Hash_set.mem visited_price_sequences curr_price_sequence)
           then (
             Hash_set.add visited_price_sequences curr_price_sequence;
             Hashtbl.update
               by_price_sequence
               curr_price_sequence
               ~f:(Option.fold ~init:curr_price ~f:( + )));
           curr_price_sequence)
    |> ignore);
  let max_sequence =
    Hashtbl.fold
      by_price_sequence
      ~init:None
      ~f:(fun ~key:price_sequence ~data:value acc ->
        Option.fold
          acc
          ~init:(price_sequence, value)
          ~f:(Comparable.max (Comparable.lift Int.compare ~f:snd))
        |> Option.some)
  in
  print_s ([%sexp_of: (Price_sequence.t * int) option] max_sequence)
;;
