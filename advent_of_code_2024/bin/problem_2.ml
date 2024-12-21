open! Core
open! Async

let level_is_safe l =
  let l_sorted = List.sort l ~compare:Int.compare in
  let arr = List.to_array l_sorted in
  let diffs_small =
    Array.for_alli arr ~f:(fun i e ->
        match i with
        | 0 -> true
        | i ->
            let last = arr.(i - 1) in
            1 <= e - last && e - last <= 3)
  in
  diffs_small
  && ([%equal: int list] l l_sorted || [%equal: int list] l (List.rev l_sorted))

let run ~filename =
  let%map safe_count =
    Reader.with_file filename ~f:(fun reader ->
        Reader.lines reader
        |> Pipe.fold ~init:0 ~f:(fun acc s ->
               let l = String.split ~on:' ' s |> List.map ~f:Int.of_string in
               let is_safe =
                 level_is_safe l
                 || List.existsi l ~f:(fun i _e ->
                        let l1, l2 = List.split_n l i in
                        l1 @ List.tl_exn l2 |> level_is_safe)
               in
               let acc = acc + if is_safe then 1 else 0 in
               return acc))
  in
  print_int safe_count
