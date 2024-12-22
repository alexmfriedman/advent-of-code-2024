open! Core
open! Async

module Tree = struct
  module T = struct
    type base =
      | Leaf of int
      | Split of t * t
    [@@deriving sexp_of]

    and t =
      { base : base [@hash.ignore] [@compare.ignore]
      ; uuid : Uuid.t
      ; size : int [@hash.ignore] [@compare.ignore]
      }
    [@@deriving hash, compare, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)

  module Pair = struct
    module T = struct
      type nonrec t = t * t [@@deriving hash, compare, sexp_of]
    end

    include T
    include Hashable.Make_plain (T)
  end

  let create_singleton stone =
    { base = Leaf stone; uuid = Uuid.create_random Random.State.default; size = 1 }
  ;;

  let create_singleton =
    Memo.general ~hashable:Int.Table.hashable (fun i -> create_singleton i)
  ;;

  let create_split =
    Memo.general ~hashable:Pair.Table.hashable (fun (t1, t2) ->
      { base = Split (t1, t2)
      ; uuid = Uuid.create_random Random.State.default
      ; size = t1.size + t2.size
      })
  ;;

  let step =
    Memo.recursive ~hashable:Table.hashable (fun step t ->
      match t.base with
      | Split (t1, t2) ->
        let t1 = step t1 in
        let t2 = step t2 in
        create_split (t1, t2)
      | Leaf stone ->
        if stone = 0
        then create_singleton 1
        else (
          let stone_string = Int.to_string stone in
          if String.length stone_string % 2 = 0
          then (
            let length = String.length stone_string / 2 in
            let s1, s2 =
              ( String.prefix stone_string length |> Int.of_string
              , String.drop_prefix stone_string length |> Int.of_string )
            in
            create_split (create_singleton s1, create_singleton s2))
          else create_singleton (stone * 2024)))
  ;;
end

let count = ref 0

let step (stones : Tree.t list) : Tree.t list =
  Core.print_s
    ([%sexp_of: int * int]
       (!count, List.sum (module Int) stones ~f:(fun tree -> tree.Tree.size)));
  count := !count + 1;
  List.map stones ~f:(fun stone -> Tree.step stone)
;;

let run ~filename =
  let%map file_contents = Reader.file_lines filename >>| List.hd_exn in
  let stones =
    String.split file_contents ~on:' '
    |> List.map ~f:Int.of_string
    |> List.map ~f:(fun i -> Tree.create_singleton i)
  in
  let stones = Fn.apply_n_times ~n:75 step stones in
  (* print_s ([%sexp_of: int list] (List.concat_map stones ~f:Tree.to_list)); *)
  print_s ([%sexp_of: int] (List.sum (module Int) stones ~f:(fun tree -> tree.Tree.size)))
;;
