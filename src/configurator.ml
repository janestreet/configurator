include Configurator.V1

let main ?(args=[]) ~name f =
  let args =
    if List.exists args ~f:(fun (key, _, _) -> key = "-ocamlc") then
      args
    else
      ("-ocamlc", String ignore, "PATH unused (deprecated)") :: args
  in
  main ~args ~name f
