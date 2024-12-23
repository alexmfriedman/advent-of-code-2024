open! Core
open! Async

let re = Re.Pcre.re "p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)" |> Re.compile

(* let grid_height = 7
let grid_width = 11 *)
let grid_height = 103
let grid_width = 101

let move_k_steps ~k ~(position : Grid_location.t) ~(velocity : Grid_location.t) =
  let row = (position.row + (velocity.row * k)) % grid_height in
  let col = (position.col + (velocity.col * k)) % grid_width in
  { Grid_location.row; col }
;;

let print_grid i (robot_locations : Grid_location.t list) =
  let as_map =
    Sequence.of_list robot_locations
    |> Sequence.map ~f:(fun grid_location -> grid_location, 1)
    |> Grid_location.Map.of_sequence_reduce ~f:( + )
  in
  (* let is_christmas_tree_count = ref 0 in
  for row = 0 to grid_height - 1 do
    for col = 0 to (grid_width / 2) - 1 do
      if
        Map.find as_map { Grid_location.row; col } |> Option.is_some
        && Map.find as_map { Grid_location.row; col = grid_width - 1 - col }
           |> Option.is_some
      then is_christmas_tree_count := !is_christmas_tree_count + 1
    done
  done;
  if !is_christmas_tree_count >= 100
  then *)
  Core.print_endline ("Attempt " ^ Int.to_string i);
  for row = 0 to grid_height - 1 do
    for col = 0 to grid_width - 1 do
      let char =
        match Map.find as_map { Grid_location.row; col } with
        | Some int -> Int.to_string int
        | None -> "."
      in
      Core.print_string char
    done;
    Core.print_endline ""
  done
;;

let move_all_k_steps ~k ~robots =
  List.map robots ~f:(fun (position, velocity) -> move_k_steps ~k ~position ~velocity)
;;

let to_quadrants robot_locations =
  let quadrant_population =
    List.filter_map robot_locations ~f:(fun position ->
      let%bind.Option horizontal_quadrant =
        match
          Int.compare position.Grid_location.col (grid_width / 2) |> Ordering.of_int
        with
        | Equal -> None
        | Less -> Some 0
        | Greater -> Some 1
      in
      let%map.Option vertical_quadrant =
        match Int.compare position.row (grid_height / 2) |> Ordering.of_int with
        | Equal -> None
        | Less -> Some 0
        | Greater -> Some 1
      in
      (2 * horizontal_quadrant) + vertical_quadrant, 1)
    |> Int.Map.of_alist_reduce ~f:( + )
  in
  (* print_s ([%sexp_of: Grid_location.t list] robot_locations);
  print_s ([%sexp_of: int Int.Map.t] quadrant_population); *)
  Map.data quadrant_population |> List.reduce_exn ~f:(fun acc x -> acc * x)
;;

let run ~filename =
  let%map file_contents = Reader.file_contents filename in
  let robots =
    Re.all re file_contents
    |> List.map ~f:(fun match_ ->
      match Re.Group.all match_ with
      | [| _; p_col; p_row; v_col; v_row |] ->
        ( { Grid_location.row = Int.of_string p_row; col = Int.of_string p_col }
        , { Grid_location.row = Int.of_string v_row; col = Int.of_string v_col } )
      | _ -> failwith "unreachable")
  in
  for i = 1600 to 10000 do
    let robot_locations = move_all_k_steps ~k:i ~robots in
    print_grid i robot_locations;
    Core.print_s ([%sexp_of: int] (to_quadrants robot_locations))
  done
;;
