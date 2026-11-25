# NeoGenera container image: build in one stage, run in a small one.
#   docker build -t neogenera .
#   docker run -it --rm neogenera --ui
FROM debian:bookworm-slim AS build
RUN apt-get update && apt-get install -y --no-install-recommends build-essential gzip python3 ca-certificates \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY . .
RUN make world docs && make install DESTDIR=/out PREFIX=/usr

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends gzip git ncurses-base \
    && rm -rf /var/lib/apt/lists/* && useradd -m neogenera
COPY --from=build /out/usr /usr
ENV TERM=xterm-256color
USER neogenera
WORKDIR /home/neogenera
ENTRYPOINT ["/usr/bin/neogenera"]
