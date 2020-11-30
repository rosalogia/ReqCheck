FROM ocaml/opam2:ubuntu
RUN opam update
RUN sudo apt-get -y install m4 pkg-config
RUN opam pin add rock.~dev https://github.com/rgrinberg/opium.git
RUN opam pin add opium.~dev https://github.com/rgrinberg/opium.git
WORKDIR reqcheck
COPY . .
RUN sudo chown -R opam:nogroup .
RUN opam repository add opam https://opam.ocaml.org
RUN opam repository remove default
RUN opam install --deps-only .
RUN eval `opam env`
RUN opam config exec dune build
EXPOSE 3000
CMD opam config exec dune exec bin/main.exe
