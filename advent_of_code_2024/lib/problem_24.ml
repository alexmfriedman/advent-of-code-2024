open! Core
open! Async
open Graph

let num_digits_inputs = 45
let _max_rand = Int.pow 2 (num_digits_inputs + 1)
let max_rand = 1000000000000

module G = Imperative.Digraph.Concrete (struct
    type t = string [@@deriving compare, hash, equal]
  end)

(* Use Core's Hashtbl to store edge labels *)
let edge_labels = Hashtbl.create (module String)

(* A module for exporting graphs to DOT with dynamic edge labels *)
module LabeledDot = struct
  include Graph.Graphviz.Dot (struct
      include G

      (* Function to name vertices *)
      let vertex_name v = "\"" ^ v ^ "\""

      (* Function to set attributes for edges *)
      let edge_attributes (src, dst) =
        let edge_key = src ^ "->" ^ dst in
        match Hashtbl.find edge_labels edge_key with
        | Some label -> [ `Label label; `Fontsize 12 ]
        | None -> []
      ;;

      (* Subgraphs are not used in this example *)
      let get_subgraph _ = None

      (* Other attributes can be customized *)
      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let default_edge_attributes _ = []
      let vertex_attributes _ = []
    end)
end

module Variable = struct
  type t = string [@@deriving equal, compare, hash, sexp_of]
end

module Operator = struct
  type t =
    | And
    | Or
    | Xor
  [@@deriving equal, compare, hash, sexp_of, string ~capitalize:"SCREAMING_SNAKE_CASE"]
end

module Bit_assignment = struct
  type t =
    | Direct of bool
    | Expr of (Operator.t * Variable.t * Variable.t)
  [@@deriving equal, compare, hash, sexp_of]
end

let eval ~inputs ~expressions =
  let values = inputs @ expressions |> String.Table.of_alist_exn in
  Memo.recursive ~hashable:String.hashable (fun eval v ->
    match Hashtbl.find_exn values v with
    | Bit_assignment.Direct bool -> bool
    | Expr (op, v1, v2) ->
      let v1 = lazy (eval v1) in
      let v2 = lazy (eval v2) in
      (match op with
       | Operator.And -> force v1 && force v2
       | Or -> force v1 || force v2
       | Xor -> Bool.equal (force v1) (force v2) |> not))
;;

let _print_graph ~expressions =
  let graph = G.create () in
  (* Add edges with dynamic labels *)
  let add_edge_with_label src dst label =
    G.add_edge graph src dst;
    let edge_key = src ^ "->" ^ dst in
    Hashtbl.set edge_labels ~key:edge_key ~data:label
  in
  List.iter expressions ~f:(fun (v, _expr) -> G.add_vertex graph v);
  List.iter expressions ~f:(fun (v, expr) ->
    match expr with
    | Bit_assignment.Direct _ -> failwith "unreachable"
    | Expr (op, v1, v2) ->
      add_edge_with_label v1 v (Operator.to_string op);
      add_edge_with_label v2 v (Operator.to_string op));
  Out_channel.with_file "graph.dot" ~f:(fun oc -> LabeledDot.output_graph oc graph)
;;

let run ~filename =
  let%map file_lines = Reader.file_lines filename in
  let empty_line_idx, _ = List.findi_exn file_lines ~f:(fun _i -> String.is_empty) in
  let _inputs =
    List.take file_lines empty_line_idx
    |> List.map ~f:(fun s ->
      match String.split s ~on:':' with
      | [ v; value ] ->
        v, Bit_assignment.Direct (String.strip value |> Int.of_string |> ( = ) 1)
      | _ -> failwith "unreachable")
  in
  let expressions =
    List.drop file_lines (empty_line_idx + 1)
    |> List.map ~f:(fun s ->
      match String.split s ~on:' ' with
      | [ v1; op; v2; "->"; v ] -> v, Bit_assignment.Expr (Operator.of_string op, v1, v2)
      | _ -> failwith "unreachable")
  in
  (* print_graph ~expressions; *)
  let outputs = List.map expressions ~f:fst |> List.sort ~compare:String.compare in
  while true do
    let x1 = Random.int max_rand in
    let x2 = Random.int max_rand in
    let desired_result = x1 + x2 in
    let inputs =
      List.init num_digits_inputs ~f:(fun i ->
        sprintf !"x%02d" i, Bit_assignment.Direct (Int.shift_right x1 i % 2 |> ( = ) 1))
      @ List.init num_digits_inputs ~f:(fun i ->
        sprintf !"y%02d" i, Bit_assignment.Direct (Int.shift_right x2 i % 2 |> ( = ) 1))
    in
    let eval = eval ~inputs ~expressions in
    let result_expanded = List.map outputs ~f:(fun v -> v, eval v) in
    let result =
      List.filter result_expanded ~f:(fun (v, _) -> String.is_prefix v ~prefix:"z")
      |> List.mapi ~f:(fun i (_, v) -> Int.shift_left (v |> Bool.to_int) i)
      |> List.sum (module Int) ~f:Fn.id
    in
    Core.print_s
      ([%sexp_of: (string * Bit_assignment.t) list * (string * bool) list]
         (inputs, result_expanded));
    if not (result = desired_result)
    then failwithf !"Input(%d, %d): Got %d, expected %d" x1 x2 result desired_result ()
  done
;;
