open Core

module Min_heap = struct
  type 'a t =
    { mutable size : int
    ; capacity : int
    ; data : 'a array
    ; compare : 'a -> 'a -> int
    }

  let parent i = (i - 1) / 2
  let left_child i = (2 * i) + 1
  let right_child i = (2 * i) + 2

  let create ~capacity ~default ~compare =
    { size = 0; capacity; data = Array.create ~len:capacity default; compare }
  ;;

  let swap heap i j =
    let temp = heap.data.(i) in
    heap.data.(i) <- heap.data.(j);
    heap.data.(j) <- temp
  ;;

  let rec heapify_up heap index =
    if index <> 0
    then (
      let p = parent index in
      if heap.compare heap.data.(index) heap.data.(p) = -1
      then (
        swap heap index p;
        heapify_up heap p))
  ;;

  let rec heapify_down heap index =
    let l = left_child index in
    let r = right_child index in
    let smallest =
      if l < heap.size && heap.compare heap.data.(l) heap.data.(index) = -1
      then l
      else index
    in
    let smallest =
      if r < heap.size && heap.compare heap.data.(r) heap.data.(smallest) = -1
      then r
      else smallest
    in
    if smallest <> index
    then (
      swap heap index smallest;
      heapify_down heap smallest)
  ;;

  let insert heap key =
    if heap.size = heap.capacity
    then failwith "Heap overflow: cannot insert into a full heap."
    else (
      heap.data.(heap.size) <- key;
      heap.size <- heap.size + 1;
      heapify_up heap (heap.size - 1))
  ;;

  let pop heap =
    if heap.size <= 0
    then failwith "Heap underflow: cannot extract from an empty heap."
    else if heap.size = 1
    then (
      heap.size <- heap.size - 1;
      heap.data.(0))
    else (
      let root = heap.data.(0) in
      heap.data.(0) <- heap.data.(heap.size - 1);
      heap.size <- heap.size - 1;
      heapify_down heap 0;
      root)
  ;;
end
