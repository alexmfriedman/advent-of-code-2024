open! Core
open! Async

module X_y = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving equal, sexp_of]

  (* rip typed_fields *)
  let ( + ) t1 t2 = { x = t1.x + t2.x; y = t1.y + t2.y }
  let ( - ) t1 t2 = { x = t1.x - t2.x; y = t1.y - t2.y }
  let ( * ) { x; y } c = { x = x * c; y = y * c }
end

let re =
  Re.Pcre.re
    "Button A: X\\+(\\d+), Y\\+(\\d+)\n\
     Button B: X\\+(\\d+), Y\\+(\\d+)\n\
     Prize: X=(\\d+), Y=(\\d+)"
  |> Re.compile
;;

let _optimize_prize ~button_a ~button_b ~prize =
  let max_button_a_presses =
    List.min_elt
      ~compare:Int.compare
      [ prize.X_y.x / button_a.X_y.x; prize.X_y.y / button_a.X_y.y ]
    |> Option.value_exn
  in
  List.init (max_button_a_presses + 1) ~f:(fun button_a_presses ->
    let button_b_diff = X_y.(prize - (button_a * button_a_presses)) in
    let solution_exists =
      button_b_diff.x % button_b.X_y.x = 0
      && button_b_diff.y % button_b.y = 0
      && button_b_diff.x / button_b.X_y.x = button_b_diff.y / button_b.y
    in
    let%map.Option () = Option.some_if solution_exists () in
    let button_b_presses = button_b_diff.x / button_b.X_y.x in
    (3 * button_a_presses) + button_b_presses)
  |> List.filter_opt
  |> List.min_elt ~compare:Int.compare
;;

(* NB: This doesn't work if the lines have the same slope *)
let optimize_prize ~(button_a : X_y.t) ~(button_b : X_y.t) ~(prize : X_y.t) =
  let x' =
    let open Float in
    (of_int prize.y - (of_int button_b.y / of_int button_b.x * of_int prize.x))
    / ((of_int button_a.y / of_int button_a.x) - (of_int button_b.y / of_int button_b.x))
    |> round ~dir:`Nearest
    |> Int.of_float
  in
  let x'' = prize.x - x' in
  let a = x' / button_a.x in
  let b = x'' / button_b.x in
  (* print_s
    ([%sexp_of: X_y.t * X_y.t * X_y.t * int * int * int * int]
       (prize, button_a, button_b, x', x'', a, b)); *)
  Option.some_if X_y.(equal prize ((button_a * a) + (button_b * b))) ((3 * a) + b)
;;

let optimize_prizes problems =
  List.sum
    (module Int)
    problems
    ~f:(fun (button_a, button_b, prize) ->
      Option.value (optimize_prize ~button_a ~button_b ~prize) ~default:0)
;;

let run ~filename =
  let%map file_contents = Reader.file_contents filename in
  let problems =
    Re.all re file_contents
    |> List.map ~f:(fun match_ ->
      match Re.Group.all match_ with
      | [| _; a_x; a_y; b_x; b_y; prize_x; prize_y |] ->
        ( { X_y.x = Int.of_string a_x; y = Int.of_string a_y }
        , { X_y.x = Int.of_string b_x; y = Int.of_string b_y }
        , { X_y.x = Int.of_string prize_x + 10000000000000
          ; y = Int.of_string prize_y + 10000000000000
          } )
      | _ -> failwith "unreachable")
  in
  let button_cost = optimize_prizes problems in
  print_s ([%sexp_of: int] button_cost)
;;
