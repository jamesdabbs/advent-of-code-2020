FROM fpco/stack-build-small

ENV STACK_ROOT /stack
RUN mkdir $STACK_ROOT

RUN apt-get update && apt-get install -y curl

WORKDIR /build/hs

COPY ./hs /build/hs
COPY ./santa /build/santa

RUN stack install --only-snapshot --jobs 4
