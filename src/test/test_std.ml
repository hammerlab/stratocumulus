

include Nonstd
module String = Sosa.Native_string
let (//) = Filename.concat



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
