
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

let ketrew_server =
  try
    if Sys.getenv "WITH_KETREW_SERVER" = "no"
    then None
    else Some (Node.make (name "ketrew-server")) 
  with _ -> 
    Some (Node.make (name "ketrew-server")) 


let configuration =
  Configuration.make ()
    ~gcloud_host
    ?get_ketrew
    ~ketrew_auth_token:(env_exn "KETREW_TOKEN")

let test_deployment =
  let nfs_mount =
    let server = Node.make (env_exn "NFS_VM") in
    Nfs.Mount.make
      ~server
      ~remote_path:(env_exn "NFS_PATH")
      ~witness:"./Hello.md"
      ~mount_point:"/nfsmain"
  in
  let compute_node name =
    Node.make name
      ~java:`Oracle_7
      ~machine_type:(`Google_cloud `Highmem_8)
      ~additional_packages:["nginx"]
  in
  Deployment.make (name "one")
    ~configuration
    ~clusters:[
      Cluster.make (name "one-cluster")
        ~compute_nodes:(
          List.init nb_of_nodes (fun i ->
              compute_node (sprintf "%s-compute-%02d" prefix i)
            )
        )
        ~open_ports:[ `Pbs_server, 80 ]
        ~nfs_mounts:[nfs_mount]
        ~torque_server:(compute_node (name "pbs-server"))
        ?ketrew_server
        ~users:[
          User.make ~unix_uid:20420 (sprintf "%s-user" prefix);
        ]
    ]

let test_nfs_deployment =
  let nfs =
    Nfs.Fresh.make (name "nfs") ~size:(`GB 200)
      ?reuse_data_disk:(env "REUSE_DATA_DISK")
  in
  let mini_cluster =
    Cluster.make (name "nfstest-minicluster")
      ~compute_nodes:[]
      ~nfs_mounts:[Nfs.Fresh.as_mount nfs ~mount_point:"/fresh-storage"]
      ~torque_server:(Node.make (name "nfstest-pbs-server"))
      ~ketrew_server:(Node.make (name "nfstest-ketrew-server"))
      ~users:[
        User.make ~unix_uid:20420 (sprintf "%s-user" prefix);
      ]
  in
  Deployment.make (name "nfstrato")
    ~configuration
    ~nfs_deployments:[ nfs ]
    ~clusters:[ mini_cluster ]

let () =
  let cmds =
    Stratocumulus.Deploy.command_line test_deployment
      ~up_command:"up"
      ~down_command:"down"
      ~print_command:"display"
      ~status_command:"status"
      ~ketrew_config_command:"ketrew-configuration"
    @
    Stratocumulus.Deploy.command_line test_nfs_deployment
      ~up_command:"nfs-up"
      ~down_command:"nfs-down"
      ~print_command:"nfs-display"
      ~status_command:"nfs-status"
      ~ketrew_config_command:"nfs-ketrew-configuration"
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
