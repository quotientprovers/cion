#
# sudo docker build -t cion -f Dockerfile .
# sudo docker run --name testcion -it cion:latest
#  # Then inside:
#  eval $(opam env)
#  cd /cion/cion
#  export ULTIMATE_HOME=/opt/UAutomizer-linux
#  dune exec cion counter
#
# sudo docker save cion:latest -o /tmp/cion-docker.tar
FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
RUN DEBIAN_FRONTEND="noninteractive" \
 && apt-get update -qy \
 && apt-get upgrade -qy \
 && apt-get install -qy software-properties-common gcc make pkg-config wget \
 && apt-get install -qy opam

# Ultimate and its dependencies
RUN apt-get install -qy openjdk-11-jdk
RUN cd /opt \ 
 && wget https://github.com/ultimate-pa/ultimate/releases/download/v0.2.1/UltimateAutomizer-linux.zip \
 && unzip UltimateAutomizer-linux.zip 

RUN opam init -y --shell=bash --shell-setup --disable-sandboxing 
RUN opam switch create 4.05.0
RUN opam install -y cil ocamlfind ocamlbuild oasis camlp4

RUN apt-get install -qy graphviz

COPY . /cion

WORKDIR /cion

RUN opam install -y dune
RUN cd /cion \
 && opam switch 4.05.0 \ 
 && eval `opam config env` \
 && cd cion \
 && dune build