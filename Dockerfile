#
# sudo docker build -t cion2 -f Dockerfile .
# sudo docker run --name testcion2 -it cion2:latest
#  # Then inside:
#  eval $(opam env)
#  cd /cion/cion
#  dune exec cion ../bench/counter.c
#
FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
RUN DEBIAN_FRONTEND="noninteractive" \
 && apt-get update -qy \
 && apt-get upgrade -qy \
 && apt-get install -qy software-properties-common gcc make pkg-config wget \
 && apt-get install -qy opam

RUN cd /opt \ 
 && wget https://github.com/ultimate-pa/ultimate/releases/download/v0.2.1/UltimateAutomizer-linux.zip \
 && unzip UltimateAutomizer-linux.zip 


RUN opam init -y --shell=bash --shell-setup --disable-sandboxing 
RUN opam switch create 4.05.0
RUN opam install -y cil ocamlfind ocamlbuild oasis camlp4

# RUN opam init -y --disable-sandboxing \
#  && opam switch create 4.05.0

# RUN  eval `opam config env` \
#  && opam install -y cil ocamlfind ocamlbuild oasis camlp4

COPY . /cion

WORKDIR /cion

RUN opam install -y dune
RUN cd /cion \
 && opam switch 4.05.0 \ 
 && eval `opam config env` \
 && cd cion \
 && dune build

RUN echo "export ULTIMATE_HOME='/opt/UAutomizer-linux'"
