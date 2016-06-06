
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
  Deployment.make "stratocumulus-test-one"
    ~configuration
    ~clusters:[
      Cluster.make "stratocumulus-test-one-cluster"
        ~compute_nodes:(
          List.init nb_of_nodes (fun i ->
              Node.make (sprintf "stratocumulus-test-compute-%02d" i)
                ~machine_type:(`GCloud "n1-highmem-8")
            )
        )
        ~nfs_server
        ~nfs_mount_point:"/nfsmain"
        ~torque_server:(Node.make "stratocumulus-test-pbs-server"
                          ~machine_type:(`GCloud "n1-highmem-8"))
        ~ketrew_server:(Node.make "stratocumulus-test-ketrew-server")
    ]

let () = Stratocumulus.Deploy.command_line test_deployment
