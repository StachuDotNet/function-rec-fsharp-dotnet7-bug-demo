FROM ubuntu:20.04@sha256:7cc0576c7c0ec2384de5cbf245f41567e922aab1b075f3e8ad565f508032df17

USER root
RUN DEBIAN_FRONTEND=noninteractive \
    apt update --allow-releaseinfo-change && \
    DEBIAN_FRONTEND=noninteractive \
    apt install \
      -y \
      --no-install-recommends \
      curl \
      apt-transport-https \
      ca-certificates \
      lsb-core \
      less \
      gpg \
      gpg-agent \
      && apt clean \
      && rm -rf /var/lib/apt/lists/*

ENV DOTNET_SDK_VERSION=7.0.100-preview.3.22179.4

RUN curl -SL --output dotnet.tar.gz https://dotnetcli.azureedge.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-sdk-$DOTNET_SDK_VERSION-linux-x64.tar.gz \
    && dotnet_sha512='ec4db3742fd9342d9f876ba3b5b15d71c2ef6e9ce57ea6f5a18c1753118b930797863cb19bb6661b5ab15fab097f8aa47294e60cf74cfbc2953d354c3e6f38cf' \
    && echo "$dotnet_sha512 dotnet.tar.gz" | sha512sum -c - \
    && mkdir -p /usr/share/dotnet \
    && tar -C /usr/share/dotnet -oxzf dotnet.tar.gz . \
    && rm dotnet.tar.gz \
    # Trigger first run experience by running arbitrary cmd
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet \
    && dotnet help