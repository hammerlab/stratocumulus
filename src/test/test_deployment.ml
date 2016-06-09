
open Nonstd
open Stratocumulus.Deploy

open Test_std


let cluster =
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
        ~nfs_mounts:[
          nfs_server, `Path "/nfsmain";
        ]
        ~torque_server:(Node.make (name "pbs-server")
                          ~machine_type:(`GCloud "n1-highmem-8"))
        ~ketrew_server:(Node.make (name "ketrew-server"))
        ~users:[
          User.make ~unix_uid:20420 (sprintf "%s-user" prefix);
        ]
    )
