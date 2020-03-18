# Devops Task

## CI/CD matters

<!-- CI status badges -->[![CircleCI, branch master](https://img.shields.io/circleci/build/bb/ulidtko/wabalabadubda/master?label=master&token=b73c13cbee07743cc9812280b34b8482adb05681)][1] [![CircleCI, branch develop](https://img.shields.io/circleci/build/bb/ulidtko/wabalabadubda/develop?label=develop&token=b73c13cbee07743cc9812280b34b8482adb05681)][2]
[1]: https://app.circleci.com/pipelines/bitbucket/ulidtko/wabalabadubda?branch=master
[2]: https://app.circleci.com/pipelines/bitbucket/ulidtko/wabalabadubda?branch=develop

**Solution highlights**

 * `stack.yaml.lock`-driven dependency caching.  
   Those commits affecting just the app code (and not touching stack.yaml) build in **2-3 minutes**. Those which do take **~30 minutes**.

 * Release image size is **112 MiB**.

### TODO

[ ] Make a better stack-build image. `fpco/stack-build{,-small}` is pathetic.
[x] Implement persistent in-file (non-mock) DB.
[x] Branch segregation for deployment.
[ ] QuickCheck-based test capable of catching the SQL injection in `Devops.Lib.DataAccess.DB`.

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

- `APP_DB_FILE`
  - Description: To set Sqlite DB filepath
  - Default Value: **main.sqlite**

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

