open! Core
open! Async

module Dir = struct
  module T = struct
    type t = { row' : int; col' : int } [@@deriving compare]
  end

  include T

  include Comparable.Make_plain (struct
    include T

    let sexp_of_t _t = Sexp.unit
  end)
end

module Solution = struct
  module T = struct
    type t = { row : int; col : int; dir1 : Dir.t; dir2 : Dir.t }
    [@@deriving compare]
  end

  include T

  include Comparable.Make_plain (struct
    include T

    let sexp_of_t _t = Sexp.unit
  end)

  let create ~row ~col ~dir1 ~dir2 =
    let dir1, dir2 = (Dir.min dir1 dir2, Dir.max dir1 dir2) in
    { row; col; dir1; dir2 }
end

let increments = List.cartesian_product [ 1; -1 ] [ 1; -1 ]
let rotate_90 ~row' ~col' = (col', -row')
let rotate_270 ~row' ~col' = (-col', row')
let is_mas = String.equal "MAS"

let word_with_midpoint_at_position_in_direction ~file_contents ~row ~col ~row'
    ~col' =
  try
    Array.init 3 ~f:(fun i ->
        let offset = i - 1 in
        file_contents.(row + (row' * offset)).(col + (col' * offset)))
    |> String.of_array |> Option.some
  with _ -> None

let run ~filename =
  let%map file_contents =
    Reader.file_lines filename >>| Array.of_list
    >>| Array.map ~f:String.to_array
  in
  let solutions = ref Solution.Set.empty in
  Array.iteri file_contents ~f:(fun row line ->
      Array.iteri line ~f:(fun col _char ->
          List.iter increments ~f:(fun (row', col') ->
              word_with_midpoint_at_position_in_direction ~file_contents ~row
                ~col ~row' ~col'
              |> Option.iter ~f:(fun word ->
                     match
                       [ rotate_90 ~row' ~col'; rotate_270 ~row' ~col' ]
                       |> List.find ~f:(fun (row'', col'') ->
                              Option.exists
                                (word_with_midpoint_at_position_in_direction
                                   ~file_contents ~row ~col ~row':row''
                                   ~col':col'') ~f:(fun word' ->
                                  is_mas word && is_mas word'))
                     with
                     | Some (row'', col'') ->
                         let solution =
                           Solution.create ~row ~col ~dir1:{ row'; col' }
                             ~dir2:{ row' = row''; col' = col'' }
                         in
                         solutions := Set.add !solutions solution
                     | None -> ())
              (*in
              if has_x then count := !count + 1*))));
  print_endline (Int.to_string (Set.length !solutions))
