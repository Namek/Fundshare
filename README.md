# Fundshare

App for sharing expenses to see current balance between people.

# Run for production

1) Run application with a database:

1. Copy `AppConfig.fs__prod` to `AppConfig.fs`
2. you may want to change ports in `docker-compose.yml`
3. `docker-compose build`
4. `docker-compose up -d`
5. http://127.0.0.1:5000 should be available then.

OR

2) Run only application (it will use local database):

1. Copy `AppConfig.fs__prod` to `AppConfig.fs` (for security, change `Auth.tokenEncryptionKey` and `Auth.passwordSalt`)
2. Set `DB.host = 'host.docker.internal'` inside `AppConfig.fs`
3. `docker build -t fundshare-app`
4. `docker run -it -p 127.0.0.1:8080:5000 fundshare-app`
5. http://127.0.0.1:8080 should be available then.


## Database

### Import

To run .sql file on database docker:

`cat dump.sql | docker exec -i fundshare_db_1 psql -U postgres -d fundshare_prod`


### Create backup

``docker exec -t -u postgres fundshare_db_1 pg_dump fundshare_prod -c > /fundshare_backup_dump_`date +%d-%m-%Y"_"%H_%M_%S`.sql``


# Development

## Frontend

* nodejs, npm
* [dotnet-script](https://github.com/filipw/dotnet-script)

```
npm i -g npx
npm i -g @dillonkearns/elm-graphql
dotnet tool install -g dotnet-script
```

Now you can:

 * `build` backend and frontend
 * `watch` for changes to build it again
 * rebuild frontend `api`, i.e. generate Elm code for GraphQL API based on `graphql_schema.json` (which is generated by backend on start)

with a command: `dotnet script build.csx api build watch`

## Backend

[.NET Core 2.1 SDK](https://www.microsoft.com/net/download)

* To build and publish an executable:

    `cd Backend && dotnet publish`

* Otherwise, you can simply open `Backend.sln` with Visual Studio 2017 and run in debug mode.
