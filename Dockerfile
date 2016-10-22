FROM ubuntu:14.04

RUN apt-get update
RUN apt-get install -qy build-essential
RUN apt-get install -qy git libgmp3-dev wget
RUN apt-get install -qy opam pkg-config m4

RUN opam init --comp 4.01.0
RUN opam update
RUN opam install -y batteries
RUN opam install -y utop

RUN apt-get install -qy libz-dev
#RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN opam install -y cryptokit
RUN opam install -y sha benchmark

ENV CAML_LD_LIBRARY_PATH "/root/.opam/4.01.0/lib/stublibs"
ENV PERL5LIB "/root/.opam/4.01.0/lib/perl5:"
ENV OCAML_TOPLEVEL_PATH="/root/.opam/4.01.0/lib/toplevel"
ENV MANPATH=":/root/.opam/4.01.0/man"
ENV PATH="/root/.opam/4.01.0/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

# ADD http://caml.inria.fr/pub/distrib/ocaml-4.01/ocaml-4.01.0.tar.gz ./
COPY ocaml-4.01.0.tar.gz ./
RUN tar -xzf ocaml-4.01.0.tar.gz

RUN apt-get install -qy emacs24-nox

RUN git clone https://github.com/amiller/lambda-auth
WORKDIR lambda-auth

ADD examples/driver_redblack.ml examples/driver_redblack.ml
ADD examples/driver.ml examples/driver.ml
ADD Makefile Makefile

RUN touch .depend
RUN make
# RUN make prover
# RUN make verifier
# RUN ./driver_prv
# RUN ./driver_vrf
RUN STRATEGY=susp make redblack #a
RUN ./redblack_prv
RUN ./redblack_vrf

RUN apt-get install -qy bc
ADD proof_size.sh ./proof_size.sh

CMD ./proof_size.sh
