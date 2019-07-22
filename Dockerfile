ARG OCAML=4.08
FROM ocaml/opam2:${OCAML}

# ADD AN OPAM REPOSITORY
RUN opam repo add --all-switches \
        roburio-git-ssh-dns-mirage3 \
        git+https://github.com/roburio/git-ssh-dns-mirage3-repo.git

# UPDATE OPAM
RUN opam update default roburio-git-ssh-dns-mirage3

# INSTALL MIRAGE CLI & LWT NATIVE DEPENDENCIES (IF ANY)
RUN opam depext mirage lwt

# INSTALL MIRAGE CLI & LWT
RUN opam install mirage lwt

# ADD THIS SOURCE DIR TO THE DOCKRE IMAGE
ADD ./ /src/

# MAKE SURE THE 'opam' USER OWNS /src
RUN sudo chown -R opam:opam /src

# STAY IN /src/mirage WHILE WE BUILD
WORKDIR /src/mirage

# PIN CALDAV
RUN opam pin caldav /src --no-action

# CONFIGURE MIRAGE UNIKERNEL
RUN opam exec mirage -- configure

# PIN MIRAGE UNIKERNEL
RUN opam pin mirage-unikernel-caldav-unix /src/mirage --no-action

# INSTALL CALDAV & MIRAGE UNIKERNEL NATIVE DEPENDENCIES (IF ANY)
RUN opam depext caldav mirage-unikernel-caldav-unix

# INSTALL CALDAV & MIRAGE OPAM DEPENDENCIES
RUN opam install caldav mirage-unikernel-caldav-unix \
        --deps-only --working-dir

# INSTALL CALDAV
RUN opam install caldav --working-dir

# BUILD THE MIRAGE UNIKERNEL
RUN opam exec mirage -- build
