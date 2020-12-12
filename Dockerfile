# See `base` branch for details
FROM quay.io/jamesdabbs/advent-of-code-2020:base

ENV STACK_ROOT /stack
RUN mkdir $STACK_ROOT

RUN apt-get update && apt-get install -y curl

WORKDIR /build/hs

COPY ./hs /build/hs
COPY ./santa /build/santa

RUN stack install --only-snapshot
