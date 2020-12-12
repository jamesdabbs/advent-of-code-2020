# See `base` branch for details
FROM quay.io/jamesdabbs/advent-of-code-2020:base

WORKDIR /build/hs

COPY ./hs /build/hs
COPY ./santa /build/santa

RUN stack install --only-snapshot
