
open Nonstd
module String = Sosa.Native_string
let (//) = Filename.concat


  module Shell_commands = struct

    let wait_until_ok ?(attempts = 10) ?(sleep = 10) cmd =
      (* Hackish way of waiting for an SSH server to be ready: *)
      sprintf "for count in $(seq 1 %d); do\n\
               sleep %d\n\
               echo \"Attempt $count\"\n\
               %s && break ||  echo 'Attempt FAILED'\n\
               done"
        attempts sleep cmd
  end


module Say = struct
  let ok x =
    printf "%s\n%!" x
  let code s =
    sprintf "```\n%s\n```" s
  let sentence s = s ^ "\n"
  let (++) = (^)
  let list f l =
    match List.map ~f l with
    | [] -> "\n"
    | _ ->
      sprintf "\n* %s\n" (String.concat  ~sep:"\n*" l)
end

module Configuration = struct
  type t = {
    gcloud_host: string [@default "/tmp/local-strato-kt"];
    compute_machine_type: string [@default "g1-small"];
    get_ketrew: [`Build | `Download_binary of string] [@default `Build];
    ketrew_auth_token: string;
    ketrew_screen_session: string [@default "ketrew-server"];
    ketrew_port: int [@default 4242];
    ketrew_configuration_path: string [@default "/tmp/ketrew-config"];
  } [@@deriving yojson, show, make]
  let gcloud_host t = Ketrew.EDSL.Host.parse t.gcloud_host
  let compute_machine_type t =
    t.compute_machine_type
  let temp_file ?(ext = ".txt") t =
    gcloud_host t
    |> Ketrew_pure.Host.playground
    |> Option.value_exn ~msg:"GCloud Host has no playground!"
    |> (fun path ->
        Ketrew_pure.Path.to_string path
        // Ketrew_pure.Internal_pervasives.Unique_id.(create ()) ^ ext)

  let get_ketrew t = t.get_ketrew

  let ketrew_auth_token t = t.ketrew_auth_token
  let ketrew_screen_session t = t.ketrew_screen_session
  let ketrew_port t = t.ketrew_port
  let ketrew_configuration_path t = t.ketrew_configuration_path
end


module Node = struct



  type t = {
    name: string [@main];
    scopes: string list [@default ["compute-rw"]];
    zone: string [@default "us-east1-c"];
    os: [ `Xenial ] [@default `Xenial];
    machine_type: [ `GCloud of string ] [@default `GCloud "g1-small"];
    java: [ `Oracle_7 | `Oracle_8 | `None ] [@default `None];
  } [@@deriving yojson, show, make, eq]

  let show t = sprintf "%s" t.name

  let image_url t =
    match t.os with
    | `Xenial ->
      "https://www.googleapis.com/compute/v1/projects/\
       ubuntu-os-cloud/global/images/ubuntu-1604-xenial-v20160429"


  let as_host t =
    ksprintf Ketrew.EDSL.Host.parse "ssh://%s/tmp/Kt-deploy/" t.name

  let sudo cmd =
    sprintf "sudo sh -c %s" (Filename.quote cmd)
  let gcloud_run_command t cmd =
    sprintf "gcloud compute ssh %s --zone %s %s" t.name t.zone
      Filename.(quote cmd)

  let sudo_if v = if v then sudo else (fun s -> s)
  let chain_gcloud ?(sudo = false) ~on cmds =
    let open Ketrew.EDSL.Program in
    chain (List.map ~f:(fun c -> sudo_if sudo c |> gcloud_run_command on |> sh) cmds)

  let destroy t ~configuration =
    let open Ketrew.EDSL in
    let host = Configuration.gcloud_host configuration in
    workflow_node without_product
      ~name:(sprintf "Destroy %s" t.name)
      ~make:(
        daemonize ~host ~using:`Python_daemon Program.(
            shf "gcloud compute instances delete %s --zone %s --quiet"
              t.name t.zone
          )
      )

  let oracle_java_ubuntu_installation ~on which_one =
    chain_gcloud ~on ~sudo:true [
      (* The default Java, OpenJDK seems to create problems
         cf. https://github.com/hammerlab/biokepi/issues/283 *)
      "add-apt-repository --yes ppa:webupd8team/java";
      "apt-get update";
      (* On top of that we have to fight with interactive licensing questions
         http://askubuntu.com/questions/190582/installing-java-automatically-with-silent-option *)
      "echo debconf shared/accepted-oracle-license-v1-1 select true | \
       debconf-set-selections";
      "echo debconf shared/accepted-oracle-license-v1-1 seen true | \
       sudo debconf-set-selections";
      begin match which_one with
      | `Oracle_7 ->
        "DEBIAN_FRONTEND=noninteractive \
         apt-get install --yes --allow-unauthenticated \
         oracle-java7-installer"
      | `Oracle_8 ->
        "DEBIAN_FRONTEND=noninteractive \
         apt-get install --yes --allow-unauthenticated \
         oracle-java8-installer"
      end;
    ]


  let create_instance t ~configuration =
    let open Ketrew.EDSL in
    let host = Configuration.gcloud_host configuration in
    let machine_type_options =
      match t.machine_type with
      | `GCloud mt -> sprintf "--machine-type %s" mt
    in
    let wait_until_really_up =
      Shell_commands.wait_until_ok (gcloud_run_command t "uname -a") in
    let make =
      daemonize ~host ~using:`Python_daemon Program.(
          shf "gcloud compute instances create %s \
               %s \
               --zone %s \
               --image %s  \
               %s"
            t.name
            (match t.scopes with
            | [] -> ""
            | _ :: _ -> "--scopes " ^ String.concat ~sep:"," t.scopes)
            t.zone
            (image_url t)
            machine_type_options
          && sh wait_until_really_up
          && chain_gcloud ~on:t ~sudo:true [
            (* We get `aptdaemon` to allow concurrent installations:
               cf. http://manpages.ubuntu.com/manpages/xenial/en/man1/aptdcon.1.html *)
            "apt-get install -y aptdaemon";
          ]

          && begin match t.java with
          | `None -> shf "echo No-java-installation"
          | `Oracle_7 | `Oracle_8 as oracle ->
            oracle_java_ubuntu_installation ~on:t oracle
          end
        ) in
    let product =
      object
        method is_done =
          Some Condition.(
              program
                ~returns:0
                ~host Program.(
                    shf "gcloud compute instances describe %s \
                         --zone %s | grep 'status: RUNNING'"
                      t.name t.zone
                  )
            )
        method node = t
      end
    in
    workflow_node product ~name:(sprintf "Create VM %s" t.name) ~make
      ~edges:[
        on_failure_activate (destroy t ~configuration)
      ]

  let ensure_software_packages t ~configuration meta_packages =
    let open Ketrew.EDSL in
    let packages =
      List.map meta_packages ~f:(fun pkg ->
          match t.os with
          | `Xenial ->
            match pkg with
            | `Nfs_client -> "nfs-common"
            | `Torque_server ->
              "torque-server torque-client torque-mom torque-pam"
            | `Torque_client -> "torque-client torque-mom"
            | `Opam -> "opam m4 pkg-config libgmp-dev"
            | `Sqlite -> "libsqlite3-dev"
            | `Biokepi_dependencies -> "cmake r-base"
            | `Libev -> "libev-dev"
        )
      |> String.concat ~sep:" "
    in
    let install_packages t p =
      match t.os with
      | `Xenial ->
        sprintf "echo yes | aptdcon --install %s" (Filename.quote p)
    in
    let host = Configuration.gcloud_host configuration in
    workflow_node without_product
      ~name:(sprintf "Install %s on %s" packages t.name)
      ~make:(
        daemonize ~host ~using:`Python_daemon Program.(
            sh (install_packages t packages |> sudo |> gcloud_run_command t)
          )
      )
      ~edges:[
        depends_on (create_instance t ~configuration)
      ]



end

module Nfs = struct


  (** This Ketrew node condition is used to make sure
      {!Nfs.Mount.ensure_server} and {!Nfs.Fresh.ensure_witness} are
      merged by equivalence when necessary. *)
  let nfs_server_present_condition ~remote_path ~witness ~host ~server =
    let open Ketrew.EDSL in
    `Is_verified Condition.(
        program ~returns:0 ~host Program.(
            sprintf "test -f %s" (remote_path // witness)
            |> Node.gcloud_run_command server |> sh
          ))

  module Mount = struct
    (** Mount existing NFS servers on nodes. *)

    type t = {
      server: Node.t;
      remote_path: string;
      witness: string; (* Relative path to a file in the NFS path *)
      mount_point: string;
    } [@@deriving yojson, show, make]

    let show {server; remote_path; mount_point; _} =
      sprintf "nfs://%s/%s?on=%s" (Node.show server) remote_path mount_point

    let mount_command {server; remote_path; mount_point; _} =
      let open Node in
      sprintf "mount -t nfs %s:%s %s" server.name remote_path mount_point

    let witness t = t.witness

    (* We check it's there but we don't set it up yet.  *)
    let ensure_server t ~configuration =
      let open Ketrew.EDSL in
      let host = Configuration.gcloud_host configuration in
      let done_when =
        nfs_server_present_condition
          ~remote_path:t.remote_path ~witness:t.witness
          ~host ~server:t.server in
      workflow_node without_product
        ~name:(sprintf "Ensure NFS server %s is alive" (show t))
        ~done_when

    let ensure nfs ~on ~configuration =
      let open Ketrew.EDSL in
      let name =
        sprintf "Mount %s on %s" (show nfs) (Node.show on) in
      let host = Configuration.gcloud_host configuration in
      let make =
        daemonize ~host ~using:`Python_daemon Program.(
            Node.chain_gcloud ~sudo:true ~on [
              sprintf "mkdir -p %s" nfs.mount_point;
              mount_command nfs;
            ]
          )
      in
      let edges = [
        depends_on (Node.ensure_software_packages on ~configuration [`Nfs_client]);
        depends_on (ensure_server nfs ~configuration);
        depends_on (Node.create_instance on ~configuration)
      ] in
      workflow_node without_product
        ~done_when:(`Is_verified (
            Condition.(
              program ~returns:0 ~host Program.(
                  sprintf "test -f %s" (nfs.mount_point // nfs.witness)
                  |> Node.gcloud_run_command on |> sh
                ))))
        ~name ~make ~edges

    let clean_up_access_rights t ~configuration =
      let open Ketrew.EDSL in
      let host = Configuration.gcloud_host configuration in
      let make =
        daemonize ~host ~using:`Python_daemon Program.(
            Node.chain_gcloud ~sudo:true ~on:t.server [
              sprintf "chmod -R 777 %s" t.remote_path;
            ]
          )
      in
      let edges = [
        depends_on (ensure_server t ~configuration);
      ] in
      workflow_node without_product ~make ~edges
        ~name:(sprintf "Chmod 777 nfs://%s:%s"
                 (Node.show t.server) t.remote_path)
  end

  module Fresh = struct
    (** Setup new NFS servers with potential data-transfers to fill them up.

        {{:https://github.com/cioc/gcloudnfs}["gcloudnfs"] repository}

    *)
    type t = {
      name: string [@main];
      witness: [ `Create of string | `Existing of string ]
          [@default `Create ".strato-witness.txt"];
      zone: string [@default "us-east1-c"];
      machine_type: [ `GCloud of string ] [@default `GCloud "n1-highmem-2"];
      size: [ `GB of int ];
    } [@@deriving yojson, show, make]

    let as_node t = Node.make (t.name ^ "-vm")

    let storage_path t = sprintf "/%s-storage" t.name

    let witness_path t =
      storage_path t // (match t.witness with `Existing p -> p | `Create p -> p)

    let as_mount t ~mount_point =
      Mount.make
        ~server:(as_node t)
        ~witness:(witness_path t |> Filename.basename)
        ~remote_path:(storage_path t)
        ~mount_point

    let create_deployment t ~configuration =
      let open Ketrew.EDSL in
      let host = Configuration.gcloud_host configuration in
      let tmp_dir = Configuration.temp_file ~ext:"-gcloudnfs.d" configuration in
      let test_liveness =
        Node.gcloud_run_command (as_node t)
          (sprintf "ls -l %s" (storage_path t)) in
      let make =
        let properties = [
          "zone", t.zone;
          "machineType", (let `GCloud g = t.machine_type in g);
          "dataDiskSizeGb", (let `GB gb = t.size in Int.to_string gb);
          "storagePoolName", t.name ^ "-storage";
          "network", "default";
          "adminPassword", t.name ^ "-password";
          "dataDiskType", "pd-standard";
        ] in
        daemonize ~host ~using:`Python_daemon Program.(
            chain [
              shf "mkdir -p %s" tmp_dir;
              shf "cd %s" tmp_dir;
              sh "wget https://github.com/cioc/gcloudnfs/archive/master.zip";
              sh "unzip master.zip";
              sh "cd gcloudnfs-master/";
              shf "gcloud deployment-manager deployments create \
                   %s --config config.jinja \
                   --properties %s"
                t.name
                (List.map properties ~f:(fun (a, b) -> a ^ "=" ^ b)
                 |> String.concat ~sep:",");
              sh (Shell_commands.wait_until_ok
                    (* We wait for the ZFS mount-point to be setup.
                       More or less 5 minutes after the command returns have
                       been observed before. *)
                    ~attempts:20 ~sleep:30 test_liveness);
            ]
          )
      in
      let condition =
        Condition.program ~host ~returns:0 Program.(sh test_liveness) in
      let name = sprintf "Create NFS deployment: %s" t.name in
      workflow_node without_product ~name ~make
        ~done_when:(`Is_verified condition)

    let ensure_witness t ~configuration =
      let open Ketrew.EDSL in
      let host = Configuration.gcloud_host configuration in
      let make =
        daemonize ~host ~using:`Python_daemon Program.(
            match t.witness with
            | `Existing _ -> chain []
            | `Create path ->
              sprintf "echo \"Created by stratocumulus on $(date -R)\" > %s"
                (storage_path t // path)
              |> Node.gcloud_run_command (as_node t)
              |> sh
          ) in
      let name = sprintf "Ensure witness file of NFS deployment: %s" t.name in
      let done_when =
        nfs_server_present_condition
          ~remote_path:(storage_path t)
          ~witness:(witness_path t |> Filename.basename)
          ~host ~server:(as_node t) in
      let edges = [
        create_deployment t ~configuration |> depends_on;
      ] in
      workflow_node without_product ~name ~make ~edges ~done_when

    let ensure t ~configuration = ensure_witness t ~configuration

    let destroy t ~configuration =
      let open Ketrew.EDSL in
      let host = Configuration.gcloud_host configuration in
      let make =
        daemonize ~host ~using:`Python_daemon Program.(
            shf
              "gcloud deployment-manager deployments delete %s -q"
              t.name
          ) in
      let name = sprintf "Destroy NFS deployment: %s" t.name in
      workflow_node without_product ~name ~make


  end
end

module Torque = struct

  (**
     https://jabriffa.wordpress.com/2015/02/11/installing-torquepbs-job-scheduler-on-ubuntu-14-04-lts/
     https://help.ubuntu.com/community/TorquePbsHowto
  *)


  let qsub_id_1 = (Random.int 10000)
  let test_qsub_condition ~on =
    let open Ketrew.EDSL in
    (* The name has to be small-ish for the hacky test to work *)
      let qn = sprintf "T%04d" qsub_id_1 in
      Condition.(
        program ~returns:0 Program.(
            (* Run a minimal test, as a USER *)
            chain [
              sprintf "echo \"sleep 42\" | qsub -N %s" qn
              |> Node.gcloud_run_command on |> sh;
              sprintf "qstat | grep %s" qn
              |> Node.gcloud_run_command on |> sh;
            ]
          )
      )

  let test_qnode_condition ~server node =
    let open Ketrew.EDSL in
    Program.(
      (* Run a minimal test, as a USER *)
      chain [
        sprintf "qnodes %s | grep 'state = free'" node.Node.name
        |> Node.gcloud_run_command server |> sh;
      ]
    )

  let setup_server ~on ~configuration =
    let open Ketrew.EDSL in
    let name = sprintf "Setup Torque server on %s" (Node.show on) in
    let host = Configuration.gcloud_host configuration in
    let make =
      let server = on.Node.name in
      daemonize ~host ~using:`Python_daemon Program.(
          chain (
            List.map ~f:(fun c -> Node.sudo c |> Node.gcloud_run_command on |> sh) [
              "/etc/init.d/torque-mom stop";
              "/etc/init.d/torque-scheduler stop";
              "/etc/init.d/torque-server stop";
              "pbs_server -t create -f";
              "killall pbs_server";
              (* Set the server hostname *)
              sprintf "echo %s > /etc/torque/server_name" server;
              sprintf "echo %s > /var/spool/torque/server_priv/acl_svr/acl_hosts" server;
              sprintf "echo root@%s > /var/spool/torque/server_priv/acl_svr/operators" server;
              sprintf "echo root@%s > /var/spool/torque/server_priv/acl_svr/managers" server;
              sprintf "echo \"%s np=1\" > /var/spool/torque/server_priv/nodes" server;
              sprintf "echo %s > /var/spool/torque/mom_priv/config" server;
              (* Now start the real one *)
              sprintf "/etc/init.d/torque-server start";
              sprintf "/etc/init.d/torque-scheduler start";
              sprintf "/etc/init.d/torque-mom start";
              (* Set scheduling properties *)
              sprintf "qmgr -c 'set server scheduling = true'";

              sprintf "qmgr -c 'set server keep_completed = 84600'";
              (* It's 24h, cf. https://wiki.archlinux.org/index.php/TORQUE *)

              sprintf "qmgr -c 'set server mom_job_sync = true'";
              "qmgr -c 'set server auto_node_np = True'";

              (* Create default queue *)
              "qmgr -c 'create queue batch'";
              "qmgr -c 'set queue batch queue_type = execution'";
              "qmgr -c 'set queue batch started = true'";
              "qmgr -c 'set queue batch enabled = true'";
              "qmgr -c 'set queue batch resources_default.walltime = 1:00:00'";
              (* "qmgr -c 'set queue batch resources_default.nodes = 1'"; *)
              "qmgr -c 'set server default_queue = batch'";
              (* Configure submission pool *)
              sprintf "qmgr -c 'set server submit_hosts = %s'" server;
              "qmgr -c 'set server allow_node_submit = true'";

            ]
          )
        )
    in
    let edges = [
      depends_on (Node.ensure_software_packages
                    on ~configuration [`Torque_server]);
      depends_on (Node.create_instance on ~configuration)
    ] in
    let condition = test_qsub_condition ~on in
    workflow_node without_product
      ~done_when:(`Is_verified condition)
      ~name ~make ~edges

  let setup_client ~on ~configuration ~server =
    let open Ketrew.EDSL in
    let name = sprintf "Setup Torque client on %s" (Node.show on) in
    let host = Configuration.gcloud_host configuration in
    let make =
      daemonize ~host ~using:`Python_daemon Program.(
          chain (
            List.map ~f:(fun c -> Node.sudo c |> Node.gcloud_run_command on |> sh) [
              sprintf "/etc/init.d/torque-mom stop";
              sprintf "echo %s > /etc/torque/server_name" server.Node.name;
              sprintf "/etc/init.d/torque-mom start";
            ]
          )
          && chain (
            List.map ~f:(fun c -> Node.sudo c |> Node.gcloud_run_command server |> sh) [
              sprintf "qmgr -c 'create node %s'" on.Node.name;
              sprintf "for count in $(seq 1 10); do\n\
                       sleep 10\n\
                       echo \"Attempt at verifying condition: $count\"\n\
                       %s && break ||  echo 'Attempt FAILED'\n\
                       done"
                (test_qnode_condition ~server on
                 |> Ketrew_pure.Program.to_single_shell_command)
            ]
          )

        )
    in
    let edges = [
      depends_on (Node.ensure_software_packages
                    on ~configuration [`Torque_client]);
      depends_on (Node.create_instance on ~configuration);
      depends_on (setup_server ~on:server ~configuration);
    ] in
    let condition =
      test_qnode_condition ~server on |> Condition.program ~returns:0 in
    workflow_node without_product
      ~done_when:(`Is_verified condition)
      ~name ~make ~edges


end


module Ketrew_server = struct

  let install ~on ~configuration =
    let open Ketrew.EDSL in
    let name = sprintf "Ensure Ketrew on %s" (Node.show on) in
    let host = Configuration.gcloud_host configuration in
    let make =
      daemonize ~host ~using:`Python_daemon Program.(
          begin match Configuration.get_ketrew configuration with
            | `Build ->
              Node.chain_gcloud ~sudo:false ~on [
                "opam init --comp=4.02.3";
                "opam install --yes tls sqlite3 conf-libev";
                "opam pin --yes add ketrew https://github.com/hammerlab/ketrew.git";
              ]
            | `Download_binary binary ->
              Node.chain_gcloud ~sudo:true ~on [
                sprintf "mkdir -p /usr/local/bin/";
                sprintf "wget %s -O/usr/local/bin/ketrew"
                  (Filename.quote binary);
                "chmod a+rx /usr/local/bin/ketrew";
              ]
          end
        )
    in
    let packages =
      match Configuration.get_ketrew configuration with
      | `Build -> [`Opam; `Sqlite; `Libev]
      | `Download_binary _ -> [`Sqlite; `Libev]
    in
    let edges = [
      depends_on (Node.ensure_software_packages on ~configuration packages);
      depends_on (Node.create_instance on ~configuration)
    ] in
    let condition =
      match Configuration.get_ketrew configuration with
      | `Build ->
        Condition.program ~returns:0 ~host Program.(
            "eval `opam config env`; ketrew --version"
            |> Node.gcloud_run_command on |> sh;
          )
      | `Download_binary _ ->
        Condition.program ~returns:0 ~host Program.(
            "ketrew --version" |> Node.gcloud_run_command on |> sh;
          )
    in
    workflow_node without_product
      ~done_when:(`Is_verified condition)
      ~name ~make ~edges

  let cert_key ~on ~configuration =
    let open Ketrew.EDSL in
    let name = sprintf "TLS cert-key pair on %s" (Node.show on) in
    let host = Configuration.gcloud_host configuration in
    let cert =
      Configuration.ketrew_configuration_path configuration // "cert.pem" in
    let key =
      Configuration.ketrew_configuration_path configuration // "privkey.pem" in
    let cert_key =
      object
        method is_done =
          Some Condition.(program ~returns:0 Program.(
              chain [
                sprintf "test -f %s" cert |> Node.gcloud_run_command on |> sh;
                sprintf "test -f %s" key |> Node.gcloud_run_command on |> sh;
              ]
            ))
        method certificate = cert
        method key = key
      end in
    let make =
      daemonize ~host ~using:`Python_daemon Program.(
          Node.chain_gcloud ~sudo:false ~on [
            sprintf "mkdir -p %s"
              (Configuration.ketrew_configuration_path configuration);
            sprintf "openssl req -x509 -newkey rsa:2048 \
                     -keyout %s -out %s \
                     -days 10 -nodes -subj \"/CN=test_ketrew\" 2> /dev/null"
              key cert;
          ]
        ) in
    workflow_node cert_key ~name ~make
      ~edges:[
        depends_on (Node.create_instance on ~configuration);
      ]

  let ketrew_configuration ~configuration ~tls_certificate ~tls_key =
    let open Ketrew.Configuration in
    let deployed file =
      Configuration.ketrew_configuration_path configuration // file in
    let debug_level = 1 in
    let engine =
      engine ~database_parameters:(deployed "db.sqlite") ()
        ~archival_age_threshold:(`Days 20.)
    in
    let server =
      server
        ~authorized_tokens:[
          authorized_token
            ~name:"configured-one"
            (Configuration.ketrew_auth_token configuration);
        ]
        ~return_error_messages:true
        ~command_pipe:(deployed "commands.pipe")
        ~engine
        (`Tls (tls_certificate, tls_key,
               Configuration.ketrew_port configuration))
    in
    [
      profile "server" (create ~debug_level server);
    ]


  let setup ?run_as ~on ~configuration () =
    let open Ketrew.EDSL in
    let name = sprintf "Setup Ketrew server on %s" (Node.show on) in
    let host = Configuration.gcloud_host configuration in
    let config_file =
      Configuration.ketrew_configuration_path configuration // "config.json" in
    let start_script =
      Configuration.ketrew_configuration_path configuration // "start.sh" in
    let tls = cert_key ~on ~configuration in
    let make =
      daemonize ~host ~using:`Python_daemon Program.(
          Node.chain_gcloud ~sudo:false ~on [
            sprintf "mkdir -p %s"
              (Configuration.ketrew_configuration_path configuration);
            sprintf "echo %s > %s"
              (ketrew_configuration ~configuration
                 ~tls_certificate:tls#product#certificate
                 ~tls_key:tls#product#key
               |> Ketrew.Configuration.to_json |> Filename.quote)
              config_file;
            sprintf "chmod -R 777 %s"
              (Configuration.ketrew_configuration_path configuration);
            sprintf "echo \"%s %s 'ketrew start-server -C %s -P server'\" > %s"
              (match Configuration.get_ketrew configuration with
              | `Build -> "eval \\`opam config env\\` ; "
              | `Download_binary _ -> "")
              (match run_as with
              | None -> "sh -c "
              | Some user -> sprintf "sudo -- su %s -c" user)
              config_file start_script;
            sprintf "chmod +x %s" start_script;
            sprintf "screen -dmS %s"
              (Configuration.ketrew_screen_session configuration);
            sprintf "screen -S %s -p 0 -X exec %s"
              (Configuration.ketrew_screen_session configuration) start_script;
          ]
        )
    in
    let edges = [
      depends_on tls;
      depends_on (install ~on ~configuration);
    ] in
    let condition =
      let status = "ps aux | grep -v grep | grep 'ketrew start-server'" in
      Condition.program ~returns:0 ~host Program.(
          status |> Node.gcloud_run_command on |> sh;
        )
    in
    workflow_node without_product
      ~done_when:(`Is_verified condition)
      ~name ~make ~edges
end


module Firewall_rule = struct

  type t = {
    name: string [@main];
    policy: [
      | `Allow_tcp of int * [ `To of string list ]
    ];
  } [@@deriving yojson, show, make]

  let ensure t ~configuration =
    let open Ketrew.EDSL in
    let cmd =
      let options =
        match t.policy with
        | `Allow_tcp (port, `To tags) ->
          sprintf "--allow tcp:%d --source-tags=%s --source-ranges 0.0.0.0/0"
            port (String.concat ~sep:"," tags)
      in
      sprintf "gcloud compute firewall-rules create %s %s" t.name options
    in
    let name = sprintf "Setup Firewall rule %s" t.name in
    let host = Configuration.gcloud_host configuration in
    let make = daemonize ~host ~using:`Python_daemon Program.(sh cmd) in
    let condition =
      Condition.program ~returns:0 ~host Program.(
          shf "gcloud compute firewall-rules describe %s" t.name
        )
    in
    workflow_node without_product
      ~done_when:(`Is_verified condition)
      ~name ~make

  let remove t ~configuration =
    let open Ketrew.EDSL in
    let cmd =
      sprintf "gcloud compute firewall-rules delete %s" t.name in
    let name = sprintf "Destroy Firewall rule %s" t.name in
    let host = Configuration.gcloud_host configuration in
    let make = daemonize ~host ~using:`Python_daemon Program.(sh cmd) in
    let condition =
      Condition.program ~returns:1 ~host Program.(
          shf "gcloud compute firewall-rules describe %s" t.name
        )
    in
    workflow_node without_product
      ~done_when:(`Is_verified condition) ~name ~make

end

module User = struct
  type t = {
    username: string [@main];
    unix_uid: int;
  } [@@deriving yojson, show, make]

  let add t ~on ~configuration =
    (* useradd or adduser?
       https://help.ubuntu.com/community/AddUsersHowto *)
    let open Ketrew.EDSL in
    let name = sprintf "Create user %s (%d)" t.username t.unix_uid in
    let host = Configuration.gcloud_host configuration in
    let make =
      daemonize ~host ~using:`Python_daemon Program.(
          Node.chain_gcloud ~sudo:true ~on [
            sprintf "adduser %s --uid %d --disabled-password"
              t.username t.unix_uid
          ]) in
    let condition =
      Condition.program ~returns:0 ~host Program.(
          Node.chain_gcloud ~sudo:false ~on [
            sprintf "groups %s" t.username
          ]
        )
    in
    workflow_node without_product
      ~done_when:(`Is_verified condition) ~name ~make
      ~edges:[
        depends_on (Node.create_instance on ~configuration);
      ]

  let ssh_key_path t =
    sprintf "/stratocumulus-ssh-keys/stratokey_%s_%d" t.username t.unix_uid

  let generate_ssh_key t ~on ~configuration =
    (* passwordless keys *)
    let open Ketrew.EDSL in
    let name =
      sprintf "Generate SSH-Key for %s (%d) on %s"
        t.username t.unix_uid (Node.show on) in
    let host = Configuration.gcloud_host configuration in
    let make =
      daemonize ~host ~using:`Python_daemon Program.(
          Node.chain_gcloud ~sudo:true ~on [
            (* The `mkdir` and the `chown` assume that there is only one
               level of directories from `$HOME`: `/.ssh/` *)
            sprintf "mkdir %s" (ssh_key_path t |> Filename.dirname);
            sprintf "ssh-keygen -t rsa -N '' -f %s" (ssh_key_path t);
            sprintf "chmod -R 755 %s" (ssh_key_path t |> Filename.dirname);
            (* sprintf "chown -R %s:%s %s"
              t.username t.username (ssh_key_path t |> Filename.dirname); *)
          ]) in
    let product =
      let condition =
        Condition.program ~returns:0 ~host Program.(
            Node.chain_gcloud ~sudo:true ~on [
              sprintf "test -f %s" (ssh_key_path t);
              sprintf "test -f %s.pub" (ssh_key_path t);
            ]
          )
      in
      object
        method is_done = Some condition
        method key = ssh_key_path t
        method pub = ssh_key_path t ^ ".pub"
      end in
    workflow_node product ~name ~make ~edges:[
        depends_on (Node.create_instance on ~configuration);
        depends_on (add t ~on ~configuration);
      ]

  let setup_ssh_key t ~from ~on ~configuration =
    let the_key = generate_ssh_key t ~on:from ~configuration in
    let open Ketrew.EDSL in
    let host = Configuration.gcloud_host configuration in
    (* cf. Node.get_gcloud_node_ssh_key *)
    let name =
      sprintf "Setup %s's SSH Keys on %s (from %s)"
        t.username (Node.show on) (Node.show from)
    in
    let make =
      daemonize ~host ~using:`Python_daemon Program.(
          (* begin match Node.equal from on with
          | false -> *)
            let tmp_dir =
              sprintf "/tmp/keys-of-%s-to-%s" from.Node.name on.Node.name in
            Node.chain_gcloud ~sudo:true ~on [
              sprintf "rm -fr ~%s/.ssh/" t.username;
              sprintf "mkdir -p ~%s/.ssh/" t.username;
              sprintf "chmod -R 777 ~%s/.ssh/" t.username;
            ]
            (* && Node.chain_gcloud ~sudo:true ~on:from [
              sprintf "chmod -R 777 ~%s/.ssh/" t.username;
              sprintf "chmod a+x ~%s/" t.username;
            ] *)
            && chain [
              (* We have to this in two steps because gcloud does not seem to be
                 able to cope with 2 distant hosts *)
              shf "mkdir -p %s" tmp_dir;
              shf "gcloud compute copy-files --zone %s %s:%s %s:%s %s/ "
                from.Node.zone
                from.Node.name the_key#product#key
                from.Node.name the_key#product#pub
                tmp_dir;
              shf "gcloud compute copy-files --zone %s %s/%s %s/%s %s:/home/%s/.ssh/"
                on.Node.zone
                tmp_dir (Filename.basename the_key#product#key)
                tmp_dir (Filename.basename the_key#product#pub)
                on.Node.name t.username;
            ]
            && Node.chain_gcloud ~sudo:true ~on [
              sprintf "chmod -R 700 ~%s/.ssh/" t.username;
            ]
          (* | true ->
            sh "echo No-SSH-key-copy-needed"
          end *)
          && Node.chain_gcloud ~sudo:true ~on [
            sprintf "echo 'IdentityFile ~%s/.ssh/%s' \
                     >> ~%s/.ssh/config"
              t.username
              (Filename.basename the_key#product#key)
              t.username;
            sprintf "echo 'StrictHostKeyChecking no' \
                     >> ~%s/.ssh/config" t.username;
            sprintf "cat ~%s/.ssh/%s >> ~%s/.ssh/authorized_keys"
              t.username
              (Filename.basename the_key#product#pub)
              t.username;
            sprintf "chown -R %s:%s ~%s/.ssh" t.username t.username t.username;
            sprintf "chmod -R 700 ~%s/.ssh" t.username;
          ]
        )
    in
    workflow_node without_product
      ~name ~make
      ~edges:[
        depends_on (Node.create_instance on ~configuration);
        depends_on the_key;
      ]

end

module Cluster = struct

  type t = {
    name: string [@main];
    compute_nodes: Node.t list;
    nfs_mounts: Nfs.Mount.t list;
    torque_server: Node.t;
    ketrew_server: Node.t;
    users: User.t list [@default []];
  } [@@deriving yojson, show, make]

  let open_ketrew_port t ~configuration =
    Firewall_rule.make (sprintf "cluster-%s-ketrew-port" t.name)
      ~policy:(`Allow_tcp (Configuration.ketrew_port configuration,
                           `To [t.ketrew_server.Node.name]))

  let all_nodes t = t.ketrew_server :: t.torque_server :: t.compute_nodes

  let up t ~configuration =
    let open Ketrew.EDSL in
    let edges =
      [
        depends_on (Firewall_rule.ensure
                      (open_ketrew_port t ~configuration) ~configuration);
        depends_on (
          Torque.setup_server ~on:t.torque_server ~configuration;
        );
        depends_on (
          Ketrew_server.setup
            ?run_as:(List.hd t.users
                     |> Option.map ~f:(fun u -> u.User.username))
            ~on:t.ketrew_server ~configuration ();
        );
      ]
      @ List.map t.nfs_mounts ~f:(fun mount ->
          depends_on (Nfs.Mount.clean_up_access_rights mount ~configuration))
      @ List.concat_map (all_nodes t) ~f:(fun on ->
          [
            depends_on (Node.ensure_software_packages on
                          ~configuration [`Biokepi_dependencies]);
            (* depends_on (Node.get_gcloud_node_ssh_key on ~configuration); *)
          ]
          @ List.map t.nfs_mounts ~f:(fun mount ->
              depends_on (Nfs.Mount.ensure mount ~on ~configuration))
          @ List.concat_map t.users ~f:(fun user ->
              [
                depends_on (User.add user ~on ~configuration);
                depends_on (User.setup_ssh_key user ~on
                              ~from:t.ketrew_server ~configuration);
              ])
        )
      @ List.map t.compute_nodes ~f:(fun on ->
          depends_on (Torque.setup_client
                        ~on ~configuration ~server:t.torque_server));
    in
    workflow_node without_product
      ~name:(sprintf "Enable cluster %s" t.name)
      ~edges

  let down t ~configuration =
    let open Ketrew.EDSL in
    let nodes_to_destroy =
      t.ketrew_server :: t.torque_server :: t.compute_nodes in
    let edges =
      depends_on (Firewall_rule.remove
                    (open_ketrew_port t ~configuration) ~configuration)
      ::
      List.map nodes_to_destroy ~f:(fun node ->
          depends_on (Node.destroy node ~configuration)
        )
    in
    workflow_node without_product
      ~name:(sprintf "Disable cluster %s" t.name)
      ~edges

  let ketrew_info t ~configuration =
    let open Pvem_lwt_unix.Deferred_result in
    let host = Configuration.gcloud_host configuration in
    let host_io = Ketrew.Host_io.create () in
    ksprintf (fun s -> Ketrew.Host_io.get_shell_command_output host_io ~host s)
      "gcloud compute instances describe %s --zone %s --format json"
      t.ketrew_server.Node.name
      t.ketrew_server.Node.zone
    >>= fun (stdout, stderr) ->
    let json = Yojson.Safe.from_string stdout in
    let in_assoc json f =
      match json with
      | `Assoc l -> List.find_map l ~f
      | other -> None in
    let ketrew_ip =
      let open Option in
      in_assoc json (function
        | "networkInterfaces", `List l -> Some l
        | other -> None)
      >>= fun netints ->
      List.find_map netints ~f:(fun ni ->
          in_assoc ni (function
            | "accessConfigs", `List l -> Some l
            | other -> None))
      >>= fun accesses ->
      List.find_map accesses ~f:(fun acc ->
          in_assoc acc(function
            | "natIP", `String ip -> Some ip
            | other -> None)
        )
    in
    let self_link =
      in_assoc json (function
        | "selfLink", `String l -> Some l
        | other -> None) in
    return (object
      method ketrew_ip = ketrew_ip
      method google_link = self_link
    end)

  let status t ~configuration =
    Lwt_main.run begin
      let open Pvem_lwt_unix.Deferred_result in
      ketrew_info t ~configuration
      >>= fun info ->
      ksprintf return "Info:\nKetrew-IP: %S\n  → https://%s:%d/gui?token=%s\n\
                       Google-link: %s\n"
        (Option.value ~default:"None" info#ketrew_ip)
        (Option.value ~default:"None" info#ketrew_ip)
        (Configuration.ketrew_port configuration)
        (Configuration.ketrew_auth_token configuration)
        (Option.value ~default:"None" info#google_link);
    end
    |> function
    | `Error (`Host _ as host_error) ->
      eprintf "ERROR:\n%s\n"
        (Ketrew.Error.to_string host_error);
      exit 2
    | `Ok s -> s

  let ketrew_client_config ?profile t ~configuration =
    let profile_name = Option.value profile ~default:t.name in
    let open Pvem_lwt_unix.Deferred_result in
    ketrew_info t ~configuration
    >>= fun info ->
    let conf =
      let open Ketrew.Configuration in
      let client =
        client
          ~token:(Configuration.ketrew_auth_token configuration)
          (sprintf "https://%s:%d"
             (Option.value_exn
                ~msg:"Could not find the IP address of the Ketrew server"
                info#ketrew_ip)
             (Configuration.ketrew_port configuration))
      in
      [
        profile profile_name (create ~debug_level:1 client);
      ]
    in
    return conf

end

module Deployment = struct

  type t = {
    name: string [@main];
    configuration: Configuration.t;
    clusters: Cluster.t list [@default []];
    nfs_deployments: Nfs.Fresh.t list [@default []];
  } [@@deriving yojson, show, make]

  let up t =
    let open Ketrew.EDSL in
    let configuration = t.configuration in
    let edges =
      List.map t.nfs_deployments
        ~f:(fun nd -> Nfs.Fresh.ensure nd ~configuration |> depends_on)
      @
      List.map t.clusters
        ~f:(fun c -> Cluster.up c ~configuration |> depends_on)
    in
    workflow_node without_product
      ~name:(sprintf "Enable deployment %s" t.name)
      ~edges

  let down t =
    let open Ketrew.EDSL in
    let configuration = t.configuration in
    let edges =
      List.map t.nfs_deployments
        ~f:(fun nd -> Nfs.Fresh.destroy nd ~configuration |> depends_on)
      @
      List.map t.clusters
        ~f:(fun c -> Cluster.up c ~configuration |> depends_on)
    in
    workflow_node without_product
      ~name:(sprintf "Disable deployment %s" t.name)
      ~edges

  let status t =
    let configuration = t.configuration in
    List.map t.clusters ~f:(fun cluster ->
        Cluster.status ~configuration cluster)
    |> String.concat ~sep:"\n"

  let output_ketrew_client_config t ~path =
    Lwt_main.run begin
      let open Pvem_lwt_unix.Deferred_result in
      let configuration = t.configuration in
      let profile =
        if List.length t.clusters = 1 then Some "default" else None in
      Pvem_lwt_unix.Deferred_list.while_sequential t.clusters ~f:(fun cluster ->
          Cluster.ketrew_client_config cluster ?profile ~configuration)
      >>| List.concat
      >>= fun config ->
      let content = Ketrew.Configuration.to_json config in
      Pvem_lwt_unix.IO.write_file path ~content
    end
    |> function
    | `Error ((`Host _ | `IO _) as error) ->
      eprintf "ERROR:\n%s\n"
        (Ketrew.Error.to_string error);
      exit 2
    | `Ok s -> s

end


let command_line
    ~up_command
    ~down_command
    ~status_command
    ~ketrew_config_command
    ~print_command
    deployment =
  let open Cmdliner in
  let sub_command ~info ~term = (term, info) in
  let workflow_command command ~descr ~f =
    sub_command
      ~term:Term.(
          pure begin fun todo ->
            let wf = f deployment in
            let prefix =
              sprintf "deployment %s → %s: "
                deployment.Deployment.name command in
            match todo with
            | `Submit ->
              Ketrew.Client.submit_workflow wf;
              Say.(
                sentence (sprintf "%ssubmitted as %s" prefix
                            Ketrew.EDSL.(node_id wf))
                |> ok
              );
            | `View ->
              Say.(
                sentence prefix
                ++ code (Ketrew.EDSL.workflow_to_string wf)
                |> ok
              );
          end
          $ Arg.(required
                 & pos 0 (some (enum ["view", `View; "submit", `Submit])) None
                 & info [] ~doc:"What to do with the Ketrew workflow: \
                                 `view` or `submit`"
                   ~docv:"WHAT-TO-DO")
        )
      ~info:Term.(info command ~doc:descr)
  in
  let up =
    workflow_command up_command
      ~descr:"The worfklow to enable the deployment"
      ~f:Deployment.up
  in
  let down =
    workflow_command down_command
      ~f:Deployment.down
      ~descr:"The worfklow to disable the deployment" in
  let format_flag =
    let open Term in
    pure (function true -> `Json | false -> `Show)
    $ Arg.(value & flag
           & info ["json"] ~doc:"Print JSON instead")
  in
  let show =
    sub_command
      ~term:Term.(
          pure begin fun format ->
            match format with
            | `Show ->
              Say.(Deployment.show deployment |> sentence |> ok);
            | `Json ->
              Deployment.to_yojson deployment
              |> Yojson.Safe.pretty_to_string
              |> printf "%s\n%!"
          end
          $ format_flag
        )
      ~info:Term.(info print_command ~doc:"Output the deployment")
  in
  let status =
    sub_command
      ~term:Term.(
          pure begin fun format ->
            match format with
            | `Show ->
              Say.(Deployment.status deployment |> sentence |> ok);
            | `Json ->
              failwith "format JSON not implemented"
          end
          $ format_flag
        )
      ~info:Term.(info status_command ~doc:"Get info from the deployment")
  in
  let ketrew_config =
    let path_arg =
      Term.(pure (fun p -> `Path p)
            $ Arg.(required & pos 0 (some string) None
                   & info [] ~doc:"The path to output to")) in
    sub_command
      ~term:Term.(
          pure begin fun (`Path path) ->
            Deployment.output_ketrew_client_config deployment ~path
          end
          $ path_arg
        )
      ~info:Term.(info ketrew_config_command
                    ~doc:"Generate a Ketrew configuration for the deployed \
                          Ketrew server")
  in
  let cmds = [
    up; down; show; status; ketrew_config;
  ]
  in
  cmds
