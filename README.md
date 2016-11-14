# purescript-google-cloud-datastore
[![Pursuit](https://pursuit.purescript.org/packages/purescript-google-cloud-datastore/badge)](https://pursuit.purescript.org/packages/purescript-google-cloud-datastore)

A wrapper library around the [Google Cloud Node.js Datastore Library](https://googlecloudplatform.github.io/google-cloud-node/#/).

WIP: At present only supports the main query and non-transactional datastore behaviour. Only tested locally with the Cloud SDK.

WIP: Does not yet support transactional behaviour.

# Getting Started

## Testing and Example

Ensure you have the [Google Cloud (Cloud SDK)](https://cloud.google.com/sdk/) installed.

You'll need to be sure to have installed the `"@google-cloud/datastore": "^0.5.0"` dependency from npm.
This is included in the package.json file for your convenience.

Ensure you export the port environment variable which is logged when running the emulator thus:

```
> gcloud beta emulators datastore start --no-legacy
```

Then run the test with:

```
> bower install
> pulp test
```

See the [test](https://github.com/jamesthompson/purescript-google-cloud-datastore/blob/master/test/Main.purs) for an example of usage.

It is recommended that you familiarise yourself with the underlying [google-cloud node.js library](https://googlecloudplatform.github.io/google-cloud-node/#/docs/google-cloud/0.43.0/datastore).

