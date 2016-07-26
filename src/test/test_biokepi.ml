

open Nonstd
module String = Sosa.Native_string
let (//) = Filename.concat


let env_exn e =
  try Sys.getenv e with
  | _ -> ksprintf failwith "Missing environment variable $%s" e
let env e =
  try Some (Sys.getenv e) with
  | _ -> None



module Pipeline (Bfx : Biokepi.EDSL.Semantics) = struct

  let reference_build = "b37"

  let bam_file_to_fastq ~path ~sample_name =
    (* TODO: move to work-dir *)
    Bfx.bam  ~sample_name ~path ~reference_build ()
    |> Bfx.bam_to_fastq ~fragment_id:"000" `PE

  let normal =
    bam_file_to_fastq
      ~path:"/nfsmain/datasets/training-dream/normal.chr20.bam"
      ~sample_name:"Training-normal"

  let tumor =
    bam_file_to_fastq ~sample_name:"Training-tumor"
      ~path:"/nfsmain/datasets/training-dream/tumor.chr20.bam"

  let vcf =
    let to_bam fq =
      Bfx.bwa_mem ~reference_build fq
      |> Bfx.picard_mark_duplicates
      |> Bfx.gatk_indel_realigner
      |> Bfx.gatk_bqsr
    in
    Bfx.strelka ()
      ~normal:(normal |> to_bam)
      ~tumor:(tumor |> to_bam)

  let qc =
    Bfx.pair (Bfx.fastqc normal) (Bfx.fastqc tumor)

(*

  let optitype ~normal_path =
    let normal = normal normal_path in
    Bfx.optitype `DNA normal
*)

  let run () =
    Bfx.observe (fun () ->
        Bfx.to_unit (Bfx.list [ Bfx.to_unit vcf; Bfx.to_unit qc ])
      )

end

module The_test_cluster = struct

  let work_dir = "/nfsmain/test-stratocumulus-with-biokepi/"

  let prefix =
    env "NAME_PREFIX" |> Option.value ~default:"stratocumulus-test"

  let host =
    ksprintf Ketrew.EDSL.Host.parse
      "ssh://%s-user@%s-pbs-server/%s/ketrew-host-playground"
      prefix prefix work_dir

  let gatk_jar_location = `Wget (env_exn "GATK_JAR_URL")
  let mutect_jar_location =
    `Fail "Please do not use Mutect 1 in stratocumulus' test"

  let max_processors = 8

  let run_program ?name ?(requirements = []) p =
    let open Ketrew.EDSL in
    let how =
      (* For now we like to abuse a bit Demeter's login node: *)
      if List.mem ~set:requirements `Quick_run
      || List.mem ~set:requirements `Internet_access
      then `On_login_node
      else `Submit_to_pbs
    in
    begin match how with
    | `On_login_node ->
      daemonize ~using:`Python_daemon ~host p
    | `Submit_to_pbs ->
      let processors =
        List.find_map requirements
          ~f:(function `Processors n -> Some n | _ -> None) in
      let name =
        Option.map name ~f:(fun n ->
            String.map n ~f:(function
              | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as c -> c
              | other -> '_')) in
      pbs ~host ?name ?processors p
    end

  let machine : Biokepi.Machine.t =
    let open Ketrew.EDSL in
    let open Biokepi.Setup.Download_reference_genomes in
    let toolkit =
        Biokepi.Setup.Tool_providers.default_toolkit ()
          ~host ~install_tools_path:(work_dir // "toolkit")
          ~run_program
          ~gatk_jar_location:(fun () -> gatk_jar_location)
          ~mutect_jar_location:(fun () -> mutect_jar_location) in
    Biokepi.Machine.create "smondet-test-cluster"
      ~max_processors
      ~get_reference_genome:(fun name ->
          Biokepi.Setup.Download_reference_genomes.get_reference_genome name
            ~toolkit
            ~host ~run_program
            ~destination_path:(work_dir // "reference-genome"))
      ~host
      ~toolkit
      ~run_program
      ~work_dir:(work_dir // "work")

end

let run () =
  let open The_test_cluster in
  let module Workflow_compiler =
    Biokepi.EDSL.Compile.To_workflow.Make(struct
      include Biokepi.EDSL.Compile.To_workflow.Defaults
      let work_dir = work_dir // "workflows"
      let machine = machine
    end)
  in
  let module Ketrew_pipeline_1 = Pipeline(Workflow_compiler) in
  let workflow_1 =
    Ketrew_pipeline_1.run ()
    |> Biokepi.EDSL.Compile.To_workflow.File_type_specification.get_unit_workflow
      ~name:"TTFI-test Pipeline"
  in
  Ketrew.Client.submit_workflow workflow_1

let () =
  match Sys.argv |> Array.to_list with
  | [one; "go"] -> run ()
  | other -> ()
