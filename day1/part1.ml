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

  let sorted_lists =
    match lists with
    | a, b ->
        ( List.sort a ~compare:(fun x y -> compare x y),
          List.sort b ~compare:(fun x y -> compare x y) )
  in

  let sum =
    let rec calculate_distance lists sum =
      match lists with
      | [], _ | _, [] -> sum
      | head1 :: tail1, head2 :: tail2 ->
          calculate_distance (tail1, tail2) (Int.abs (head1 - head2) + sum)
    in
    calculate_distance sorted_lists 0
  in

  Out_channel.output_string Out_channel.stdout (Int.to_string sum)
