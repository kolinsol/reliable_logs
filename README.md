# Reliable logs

Reliable logs is an application for storing logs coming from any other application over HTTP. The application is written in Erlang and uses PostgreSQL as its underlying datastore.

#### Prerequisites

In order for this application to work you need to have several things installed on your machine:
* [Erlang](https://www.erlang.org/) - Implementation language and environment
* [PostgreSQL](https://www.postgresql.org/) - Underlying database
* [rebar3](https://www.rebar3.org/) - Erlang build tool

This application was designed to work specifically with Erlang _**18.3**_ and PostgreSQL _**10.5**_ but it *might* also work for other versions.

To install a specific version of Erlang you might want to consider using [kerl](https://github.com/kerl/kerl)

#### Installing
###### clone the repository
```
git clone git@github.com:kolinsol/reliable_logs.git
```
###### enter the newly created directory
```
cd reliable_logs
```
###### Start the database server
If you are using macOS (like me) and you have postgres installed via homebrew
then you can just do
```
brew services start pstgres
```
Otherwise please consult [this page](https://www.postgresql.org/docs/9.1/static/server-start.html)
###### create database structure
create the database using the provided *init.sql* file
```
psql < init.sql
```
###### run the application
```
rebar3 run
```
The application will start on port **8080**
To ensure the application works you can issue the following request^
```
curl http://localhost:8080/ping
```
If application is working you'll get a response like this^
```
{"success": true}
```
