FROM quay.io/haskell_works/ghc-8.6.5
LABEL MAINTAINER Alexey Raga <alexey.raga@gmail.com>

RUN apt-get update && apt-get install -y \
    ca-certificates \
    # .NET Core dependencies
    #krb5-libs libgcc libintl libssl1.0 libstdc++ lttng-ust userspace-rcu zlib\
    build-essential \
    libkrb5-dev \
    libc6-dev  \
    libssl1.0-dev \
    libstdc++6 \
    liblttng-ust0 \
    tzdata \
    liburcu-dev \
    zlib1g-dev

# Configure web servers to bind to port 80 when present
ENV ASPNETCORE_URLS=http://+:80 \
# Enable detection of running in a container
    DOTNET_RUNNING_IN_CONTAINER=true \
# Set the invariant mode since icu_libs isn't included (see https://github.com/dotnet/announcements/issues/20)
    DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=true \
    DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true \
    DOTNET_CLI_TELEMETRY_OPTOUT=true

ENV AzureWebJobsScriptRoot=/home/site/wwwroot \
    HOME=/home \
    FUNCTIONS_WORKER_RUNTIME=swift \
    languageWorkers__workersDirectory=/home/site/wwwroot/workers

CMD ["/bin/true"]

# ENV DOTNET_SKIP_FIRST_TIME_EXPERIENCE true
# ENV DOTNET_CLI_TELEMETRY_OPTOUT true

# RUN apt-get update \
#     && apt-get upgrade -y \
#     && apt-get install -y wget unzip apt-transport-https gnupg \
#     && wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.asc.gpg \
#     && mv microsoft.asc.gpg /etc/apt/trusted.gpg.d/ \
#     && wget -q https://packages.microsoft.com/config/debian/9/prod.list \
#     && mv prod.list /etc/apt/sources.list.d/microsoft-prod.list \
#     && apt-get update \
#     && wget https://github.com/google/protobuf/releases/download/v3.6.1/protoc-3.6.1-linux-x86_64.zip \
#     && unzip protoc-3.6.1-linux-x86_64.zip -d /usr \
#     && rm protoc-3.6.1-linux-x86_64.zip \
#     && apt-get install -y dotnet-sdk-2.2 \
#     && apt-get remove -y --purge wget unzip apt-transport-https gnupg \
#     && apt-get autoremove -y \
#     && apt-get clean \
#     && rm -rf /usr/share/dotnet/sdk/NuGetFallbackFolder/*

# WORKDIR /root

# CMD ["/bin/true"]
