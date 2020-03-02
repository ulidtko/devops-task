# Devops Task

## Configuration

There are a couple of environment variables it can be provided in order to change Server configuration:

- `APP_API_PORT`
  - Description: To Change the port number the API is going to listen to
  - Default Value: **3000**

- `APP_API_BASIC_USER_NAME`
  - Description: To set User Name for Basic Auth
  - Default Value: **user**

- `APP_API_BASIC_PASSWORD`
  - Description: To set User Password for Basic Auth
  - Default Value: **pass**

## Run

### Prerequisites

In order to run this solution you are going to need the following distributions installed.

- Stack 2.1

### Run tests

```bash
stack test
```

### Startup Server

```bash
stack build
stack exec devops-api
```


### Startup Server with Other Configuration

```bash
stack build
APP_API_PORT=8080 \
APP_API_BASIC_USER_NAME=myuser \
APP_API_BASIC_PASSWORD=mypass \
stack exec devops-api
```

## Test Server

```bash
curl -v -X POST http://localhost:3000/api/ -d '{"jsonrpc": "2.0", "id": 0, "method": "get_raw_transactions", "params": { "address": "SOMEADDRESS" } }' -H 'Content-Type: application/json' -u user:pass
```

## Test examples with Mocked Data

### Get Raw Transactions

```bash
curl -v -X POST http://localhost:3000/api/ -d '{"jsonrpc": "2.0", "id": 0, "method": "get_raw_transactions", "params": { "address": "ADDRESS_C" } }' -H 'Content-Type: application/json' -u user:pass
```

### Get Normalized Balances

```bash
curl -v -X POST http://localhost:3000/api/ -d '{"jsonrpc": "2.0", "id": 0, "method": "get_normalized_balances", "params": { "addresses": ["ADDRESS_B"] } }' -H 'Content-Type: application/json' -u user:pass
```

