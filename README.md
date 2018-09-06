# Reliable logs

Reliable logs is an application for storing logs coming from any other application over HTTP. The application is written in Erlang and uses PostgreSQL as its underlying datastore.

## Prerequisites

In order for this application to work you need to have several things installed on your machine:
* [Erlang](https://www.erlang.org/) - Implementation language and environment
* [PostgreSQL](https://www.postgresql.org/) - Underlying database
* [rebar3](https://www.rebar3.org/) - Erlang build tool

This application was designed to work specifically with Erlang _**18.3**_ and PostgreSQL _**10.5**_ but it *might* also work for other versions.

To install a specific version of Erlang you might want to consider using [kerl](https://github.com/kerl/kerl)

## Installing
#### clone the repository
```
git clone git@github.com:kolinsol/reliable_logs.git
```
#### enter the newly created directory
```
cd reliable_logs
```
#### compile the project
```
rebar3 compile
```
#### run tests
```
rebar3 ct
```
#### Start the database server
If you are using macOS (like me) and you have postgres installed via [homebrew](https://brew.sh/)
then you can just do
```
brew services start postgres
```
Otherwise please consult [this page](https://www.postgresql.org/docs/9.1/static/server-start.html)
#### create database structure
create the database using the provided *init.sql* file
```
psql < init.sql
```
#### run the application
```
rebar3 run
```
The application will start on port **8080**
To ensure the application works you can issue the following command
in the command line:
```
curl http://localhost:8080/ping
```
If application is working you'll get a response like this:
```
{"success": true}
```
## API
The api has two publicly exposes endoints
* /insert
* /select
Both of the endpints accept *POST* requests and work only with *JSON* data
#### insert
Each request to */insert* endpoint should encode in its body a single log string

The body should have the following structure:
```json
{
	"log_created": "20120203T040506",
	"app_id": "journalctl",
	"object_id": 12345678934567,
	"tags": {"severity": "crititcal"},
	"message": "out of memory",
	"context": {"additional_info": "some additional info"}
}
```
here is the description of each field:

|field name|required|type|description|
|-|-|-|-|
|log_created|true|timestamp|event occurence time|
|app_id|true|string|application identifier|
|object_id|true|int8|unique identifier of an outer object|
|tags|true|json|arbitrary log classification info|
|message|true|string|human readable event description|
|context|false|json|any extra info|

> please note that *timestamp* fields must be formatted according to [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) standard 
#### select
each request to */select* should be formed like this:
```json
{
	"query": "some SQL"
}
```
> the application will try to execute any query contained in the request's body
> but onlu **select** on only **logs** table is allowed on the database level
#### example
lets issue a basic select request
```
curl --header "Content-Type: application/json" \
     --request POST \
     --data '{"query":"select * from logs"}' \
     http://localhost:8080/select
```
as a result we can get something like this:
``` json
{
    "success": true,
    "result": [
        {
            "request_id": "1",
            "log_created": "2012-02-03 04:05:06",
            "created": "2018-09-03 21:48:47.47922",
            "app_id": "aaa",
            "object_id": "12345678934567",
            "tags": "{\"a\": 2}",
            "message": "dwddw",
            "context": "{\"s\": 10}"
        },
        {
            "request_id": "2",
            "log_created": "2012-02-03 04:05:06",
            "created": "2018-09-04 11:40:03.348056",
            "app_id": "aaa",
            "object_id": "12345678934567",
            "tags": "{\"a\": 2}",
            "message": "dwddw",
            "context": "{\"s\": 10}"
        },
        {
            "request_id": "3",
            "log_created": "2012-02-03 04:05:06",
            "created": "2018-09-04 11:43:38.044312",
            "app_id": "aaa",
            "object_id": "12345678934567",
            "tags": "{\"a\": 2}",
            "message": "dwddw",
            "context": "{\"s\": 10}"
        }
    ]
}
```
> note that here we have two additional fields geberated by the database in each json
* created - time of insertion into the database
* request_id - unique identifier of each record

