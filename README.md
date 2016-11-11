# purescript-google-cloud-datastore

A wrapper library around the [Google Cloud Node.js Datastore Library](https://googlecloudplatform.github.io/google-cloud-node/#/).

WIP: At present only supports the main query and non-transactional datastore behaviour. Only tested locally with the Cloud SDK.

WIP: Does not yet support transactional behaviour.

## Testing

Ensure you have the [Google Cloud (Cloud SDK)](https://cloud.google.com/sdk/) installed.

Ensure you export the port information logged when running the emulator thus:

```
> gcloud beta emulators datastore start --no-legacy
```

Then run the test with:

```
> pulp test
```
