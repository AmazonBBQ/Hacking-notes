FROM ocaml/opam:latest 
RUN sudo apt-get update && sudo apt-get install -y libsqlite3-dev sqlite3 libev-dev libgmp-dev pkg-config openssl libssl-dev

RUN opam install dune
RUN opam install dream
RUN opam install yojson
RUN opam install sqlite3

WORKDIR /app

ARG FLAG=PWNED{FAKE_FLAG}
RUN echo "$FLAG" > /app/flag.txt

COPY plugins/ plugins/
COPY dune-project .
COPY bin/ bin/
COPY ppx/ ppx/

RUN opam exec -- dune build bin/init_db.exe
RUN opam exec -- dune exec bin/init_db.exe

RUN opam exec -- dune build bin/http.exe

CMD [ "/app/_build/default/bin/http.exe" ]