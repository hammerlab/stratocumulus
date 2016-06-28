FROM ocaml/opam:ubuntu-15.10_ocaml-4.02.3
#RUN sudo apt-get install --yes libsqlite3-dev libev-dev libgmp-dev
RUN opam install -y tls
RUN opam pin --yes add ketrew https://github.com/hammerlab/ketrew.git
RUN opam pin --yes add biokepi https://github.com/hammerlab/biokepi.git
RUN opam pin --yes add stratocumulus https://github.com/hammerlab/stratocumulus.git
