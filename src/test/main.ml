
let () =
  let cmds =
    Stratocumulus.Deploy.command_line Test_deployment.cluster
      ~up_command:"up"
      ~down_command:"down"
      ~print_command:"display"
      ~status_command:"status"
      ~ketrew_config_command:"ketrew-configuration"
  in
  let open Cmdliner in
  let version = Stratocumulus.Metadata.version |> Lazy.force in
  let sub_command ~info ~term = (term, info) in
  let submit_biokepi_workflow =
    sub_command
      ~info:Term.(info "biokepi-test" ~doc:"The Example Biokepi workflow")
      ~term:Term.(
          pure begin function
            | `Submit -> Test_biokepi.run ()
            | `View -> failwith "biokepi-test: View -> TODO"
          end
          $ Arg.(required
                 & pos 0 (some (enum ["view", `View; "submit", `Submit])) None
                 & info [] ~doc:"What to do with the Ketrew workflow: \
                                 `view` or `submit`"
                   ~docv:"WHAT-TO-DO")
        ) in
  let default_cmd =
    let doc = "Some workflows to setup google cloud clusters" in
    let man = [
      `S "AUTHORS";
      `P "Sebastien Mondet <seb@mondet.org>"; `Noblank;
      `S "BUGS";
      `P "Browse and report new issues at"; `Noblank;
      `P "<https://github.com/hammerlab/stratocumulus>.";
    ] in
    sub_command
      ~term:Term.(ret (pure (`Help (`Plain, None))))
      ~info:(Term.info Sys.argv.(0) ~version ~doc ~man) in
  match Term.eval_choice default_cmd (submit_biokepi_workflow :: cmds) with
  | `Ok f -> f
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
