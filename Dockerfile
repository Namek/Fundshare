FROM fsharp:10.2.1-netcore AS build

WORKDIR /bin
RUN curl -sL https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz --output elm.gz && \
    gzip -d elm.gz && chmod +x elm && \
    dotnet tool install -g dotnet-script --version 0.25.0 && \
    ln -sfn /root/.dotnet/tools/dotnet-script /bin/dotnet-script

WORKDIR /app/
COPY Backend/*.fsproj ./
RUN dotnet restore

COPY Backend/AppConfig.fs ./AppConfig.fs
COPY Backend/*.fs ./
COPY Backend/Utils/*.fs ./Utils/
RUN dotnet publish -c Release -o out

WORKDIR /frontend/
COPY Frontend/src/ ./src
RUN false | cp -i ./src/elm/I18n/I18n.elm.template ./src/elm/I18n/I18n.elm
COPY Frontend/build.csx ./
RUN rm -rf ./src/elm/elm-stuff && \
    dotnet script build.csx build

FROM microsoft/dotnet:2.1-runtime AS runtime
WORKDIR /app

# backend build: dotnet publish
COPY --from=build /app/out ./

# frontend build: build.csx output (css, elm->js, font, images)
COPY --from=build /frontend/public ./public/

EXPOSE 5000
ENTRYPOINT ["dotnet", "Backend.dll"]

