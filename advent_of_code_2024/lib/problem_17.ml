open! Core
open! Async

let re =
  Re.Pcre.re
    "Register A: (\\d+)\n\
     Register B: (\\d+)\n\
     Register C: (\\d+)\n\n\
     Program: (\\d(?:,\\d)*)"
  |> Re.compile
;;

module Registers = struct
  type t =
    { mutable a : int
    ; mutable b : int
    ; mutable c : int
    }
  [@@deriving sexp_of]
end

let run_program ~(registers : Registers.t) ~program =
  let rec loop instruction_pointer =
    if instruction_pointer < Array.length program
    then (
      let opcode = program.(instruction_pointer) in
      let literal_operand = program.(instruction_pointer + 1) in
      let combo_operand =
        match literal_operand with
        | 0 | 1 | 2 | 3 -> literal_operand
        | 4 -> registers.a
        | 5 -> registers.b
        | 6 -> registers.c
        | 7 -> failwith "Reserved"
        | _ -> failwith "unreachable"
      in
      let instruction_pointer' = ref (instruction_pointer + 2) in
      (match opcode with
       | 0 -> registers.a <- registers.a / Int.pow 2 combo_operand
       | 1 -> registers.b <- Int.bit_xor registers.b literal_operand
       | 2 -> registers.b <- combo_operand % 8
       | 3 ->
         (match registers.a with
          | 0 -> ()
          | _ -> instruction_pointer' := literal_operand)
       | 4 -> registers.b <- Int.bit_xor registers.b registers.c
       | 5 -> printf !"%d," (combo_operand % 8)
       | 6 -> registers.b <- registers.a / Int.pow 2 combo_operand
       | 7 -> registers.c <- registers.a / Int.pow 2 combo_operand
       | _ -> failwith "unreachable");
      loop !instruction_pointer')
    else print_newline ()
  in
  loop 0
;;

let run ~filename =
  let%map file_contents = Reader.file_contents filename in
  match Re.exec re file_contents |> Re.Group.all with
  | [| _; register_a; register_b; register_c; program |] ->
    let register_a = Int.of_string register_a in
    let register_b = Int.of_string register_b in
    let register_c = Int.of_string register_c in
    let program =
      String.split program ~on:',' |> List.map ~f:Int.of_string |> List.to_array
    in
    run_program
      ~registers:{ Registers.a = register_a; b = register_b; c = register_c }
      ~program
  | _ -> failwith "unreachable"
;;
