#!/bin/bash

apt-get install opam
opem init -y
eval $(opam env)
opam install ocaml-lsp-server odoc ocamlformat utop
opam switch create hardcaml 4.13.1
opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
opam install hardcaml hardcaml_waveterm ppx_jane ppx_expect ppx_deriving_hardcaml
