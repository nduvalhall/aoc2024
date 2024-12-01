open Core

let () =
  let lines = In_channel.read_lines "day1/input.txt" in

  let lists =
    let rec make_lists lines list1 list2 =
      match lines with
      | [] -> (list1, list2)
      | head :: tail ->
          let split =
            String.split ~on:' ' head
            |> List.filter ~f:(fun s -> not (String.equal s ""))
          in
          make_lists tail
            (Int.of_string (List.nth_exn split 0) :: list1)
            (Int.of_string (List.nth_exn split 1) :: list2)
    in

    make_lists lines [] []
  in

  let sum =
    let rec calculate_similarity_score lists sum =
      match lists with
      | [], _ | _, [] -> sum
      | head :: tail, other ->
          let score = List.count other ~f:(fun x -> Int.equal head x) in
          calculate_similarity_score (tail, other) ((head * score) + sum)
    in
    calculate_similarity_score lists 0
  in

  Out_channel.output_string Out_channel.stdout (Int.to_string sum)
