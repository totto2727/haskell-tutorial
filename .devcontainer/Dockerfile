FROM haskell:8.10.2-buster

RUN apt update \
    && apt install -y --no-install-recommends \
    && apt-get autoremove -y \ 
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*