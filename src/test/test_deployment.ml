
open Nonstd
open Stratocumulus.Deploy

let env_exn e =
  try Sys.getenv e with
  | _ -> ksprintf failwith "Missing environment variable $%s" e
let env e =
  try Some (Sys.getenv e) with
  | _ -> None

let gcloud_host = env_exn "GCLOUD_HOST"
let get_ketrew =
  Option.map ~f:(fun url -> `Download_binary url) (env "KETREW_BIN")

let nb_of_nodes =
  env "NODES" |> Option.map ~f:int_of_string |> Option.value ~default:2

let prefix =
  env "NAME_PREFIX" |> Option.value ~default:"stratocumulus-test"

let name s = sprintf "%s-%s" prefix s

let test_deployment =
  let configuration =
    Configuration.make ()
      ~gcloud_host
      ?get_ketrew
      ~ketrew_auth_token:(env_exn "KETREW_TOKEN")
  in
  let nfs_server =
    let server = Node.make (env_exn "NFS_VM") in
    Nfs.make
      ~server
      ~remote_path:(env_exn "NFS_PATH")
      ~witness:"./Hello.md"
  in
  Deployment.make (name "one")
    ~configuration
    ~cluster:(
      Cluster.make (name "one-cluster")
        ~compute_nodes:(
          List.init nb_of_nodes (fun i ->
              Node.make (sprintf "%s-compute-%02d" prefix i)
                ~machine_type:(`GCloud "n1-highmem-8")
            )
        )
        ~nfs_server
        ~nfs_mount_point:"/nfsmain"
        ~torque_server:(Node.make (name "pbs-server")
                          ~machine_type:(`GCloud "n1-highmem-8"))
        ~ketrew_server:(Node.make (name "ketrew-server"))
    )

let () =
  let cmds =
    Stratocumulus.Deploy.command_line test_deployment
      ~up_command:"up"
      ~down_command:"down"
      ~print_command:"display"
      ~status_command:"status"
      ~ketrew_config_command:"ketrew-configuration"
  in
  let open Cmdliner in
  let version = Stratocumulus.Metadata.version |> Lazy.force in
  let sub_command ~info ~term = (term, info) in
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
  match Term.eval_choice default_cmd cmds with
  | `Ok f -> f
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
