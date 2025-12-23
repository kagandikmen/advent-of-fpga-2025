#!/bin/bash

apt-get install opam -y
opam init -y
opam switch create hardcaml 4.13.1
eval $(opam env --switch=hardcaml)
opam install -y ocaml-lsp-server odoc ocamlformat utop
opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository -y
opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages -y
opam install -y hardcaml hardcaml_waveterm ppx_jane ppx_expect ppx_deriving_hardcaml
