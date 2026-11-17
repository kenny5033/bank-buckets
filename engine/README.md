# Bank Buckets Engine

## Running

- JSON-RPC server
```bash
cabal run --ghc-options="-threaded" bank-buckets jsonrpc
```

- CLI
```bash
cabal run bank-buckets cli
```

## Helpful Things

- Forward domain socket traffic to a port
```bash
socat TCP-LISTEN:8888,fork UNIX-CONNECT:$HOME/.cache/bankbuckets/jsonrpc.sock
```
