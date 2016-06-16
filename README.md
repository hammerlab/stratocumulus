Stratocumulus
=============

Deploy biggish cloud-based clusters.

Run The Tests
-------------

The main test is in two pieces, UX will improve over time:

- `src/test/test_deployment.ml` → uses the `stratocumulus` library to define a
  cluster deployment.
- `src/test/test_biokepi.ml` → uses the deployed cluster to run a Biokepi
  workflow.

For the Biokepi submission to work Ketrew should be compiled with TLS support
(`opam install tls`).

Cloud Requirements:

- a [“One-click NFS server”](https://cloud.google.com/launcher/docs/single-node-fileserver).
- a host with `gcloud` installed and configured **and** password-less
  `~/.ssh/google_compute_engine` ssh-keys (`$GCLOUD_HOST` below).
  (also make sure some basic utilities are there like `unzip`, `wget`, …)

The test is configured with environment variables:

```sh
# Ketrew host that has the `gcloud` tool:
export GCLOUD_HOST=ssh://MyGCloudSSHHost/tmp/KT

# (Optional) Ketrew binary for Ubuntu Xenial (Ketrew will be built if absent).
export KETREW_BIN="http://example.com/path/to/ketrew-xenial-x86_64"

# (Optional) A prefix for all the names creates (compute nodes and servers):
export NAME_PREFIX="my-first-deplyment"

# (Optional) The number of compute nodes in the cluster:
export NODES=12

# Authentication token for the Ketrew server to deploy:
export KETREW_TOKEN=kjkjdjedekj388787732edkde8ude909e39iki

# The NFS server:
export NFS_VM="stratocumulus-test-nfs-server-vm"
export NFS_PATH="/some-test-storage"

# The URL to download the GATK:
export GATK_JAR_URL="http://example.com/path/to/GenomeAnalysisTK_35.jar"
```

Start the deployment workflow (a “deployer“ Ketrew server is assumed to be
configured/running):

    ./deployment-test up view

When the workflow is done and successful:

    ./deployment-test status

displays the URL to the WebUI of the Ketrew server; and

    ./deployment-test ketrew-config some-file.json

outputs a valid Ketrew client-configuration to `some-file.json`.

One can now run the Biokepi workflow:

    KETREW_CONFIG=some-file.json ./biokepi-test go
