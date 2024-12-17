open! Core

type state =
  { register_a : int
  ; register_b : int
  ; register_c : int
  ; initial_program : int list
  ; program : int list
  ; output : int list
  }

let operand_value state operand =
  match operand with
  | 0 | 1 | 2 | 3 -> operand
  | 4 -> state.register_a
  | 5 -> state.register_b
  | 6 -> state.register_c
  | 7 -> failwith "Reserved operand: 7"
  | _ -> failwith (sprintf "Unknown operand %d" operand)
;;

let run_program state opcode operand rest =
  match opcode with
  | 0 ->
    { state with
      register_a = state.register_a / Int.pow 2 (operand_value state operand)
    ; program = rest
    }
  | 1 ->
    { state with
      register_b = Int.bit_xor state.register_b operand
    ; program = rest
    }
  | 2 ->
    { state with register_b = operand_value state operand % 8; program = rest }
  | 3 ->
    (match state.register_a with
     | 0 -> { state with program = rest }
     | _ ->
       let program = List.drop state.initial_program (operand - 1) in
       { state with program })
  | 4 ->
    { state with
      register_b = Int.bit_xor state.register_b state.register_c
    ; program = rest
    }
  | 5 ->
    { state with
      output = (operand_value state operand % 8) :: state.output
    ; program = rest
    }
  | 6 ->
    { state with
      register_b = state.register_a / Int.pow 2 (operand_value state operand)
    ; program = rest
    }
  | 7 ->
    { state with
      register_c = state.register_a / Int.pow 2 (operand_value state operand)
    ; program = rest
    }
  | _ -> failwith (sprintf "Unknown opcode %d" opcode)
;;

let rec run_forever state =
  match state.program with
  | [] | [ _ ] -> List.rev state.output
  | opcode :: operand :: rest ->
    let state = run_program state opcode operand rest in
    run_forever state
;;

let _part_1 =
  let initial_state =
    { register_a = 25358015
    ; register_b = 0
    ; register_c = 0
    ; initial_program = [ 2; 4; 1; 1; 7; 5; 0; 3; 4; 7; 1; 6; 5; 5; 3; 0 ]
    ; program = [ 2; 4; 1; 1; 7; 5; 0; 3; 4; 7; 1; 6; 5; 5; 3; 0 ]
    ; output = []
    }
  in
  run_forever initial_state
;;

let rec is_same a b =
  match a, b with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 -> if h1 = h2 then is_same t1 t2 else false
  | _ -> false
;;

let () =
  let rec increment a =
    printf "a = %d\r" a;
    let state =
      { register_a = a
      ; register_b = 0
      ; register_c = 0
      ; initial_program = [ 2; 4; 1; 1; 7; 5; 0; 3; 4; 7; 1; 6; 5; 5; 3; 0 ]
      ; program = [ 2; 4; 1; 1; 7; 5; 0; 3; 4; 7; 1; 6; 5; 5; 3; 0 ]
      ; output = []
      }
    in
    let output = run_forever state in
    if is_same output state.initial_program then a else increment (a + 1)
  in
  let s = increment 1111111111111111 in
  printf "a = %d" s
;;
