FROM fsharp:10.2.1-netcore AS build

WORKDIR /bin
RUN curl -sL https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-linux.tar.gz --output elm.tar.gz && \
    tar -xzf elm.tar.gz && rm elm.tar.gz && \
    dotnet tool install -g dotnet-script && \
    ln -sfn /root/.dotnet/tools/dotnet-script /bin/dotnet-script

WORKDIR /app/
COPY FundshareBackend/fsc.props FundshareBackend/*.fsproj ./
RUN dotnet restore

COPY FundshareBackend/Config.yaml ./Config.yaml
COPY FundshareBackend/*.fs ./
COPY FundshareBackend/Utils/*.fs ./Utils/
RUN dotnet publish -c Release -o out

WORKDIR /frontend/
COPY FundshareFrontend/src/ ./src
COPY FundshareFrontend/build.csx ./
RUN rm -rf ./src/elm/elm-stuff && \
    dotnet script build.csx build

FROM microsoft/dotnet:2.1-runtime AS runtime
WORKDIR /app

# backend build: dotnet publish
COPY --from=build /app/out ./

# frontend build: build.csx output (css, elm->js, font, images)
COPY --from=build /frontend/public ./public/

EXPOSE 5000
ENTRYPOINT ["dotnet", "FundshareBackend.dll"]

