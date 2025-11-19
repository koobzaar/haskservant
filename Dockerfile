FROM haskell:9.6.7 AS build

WORKDIR /app

COPY haskservant.cabal cabal.project ./  

RUN cabal update

COPY . .

RUN cabal build --enable-relocatable

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y libgmp10 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/dist-newstyle/build/*/*/haskservant-*/x/haskservant/build/haskservant .

EXPOSE 8080

CMD ["/app/haskservant"]
