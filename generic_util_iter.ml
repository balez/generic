type exn += False

let for_all_in a b f =
  let r = ref true in
  begin
    try
      for i = a to b do
        r := !r && f i;
        if not !r then raise False
      done;
      !r
    with False -> false
  end
