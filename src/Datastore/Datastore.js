"use strict";

exports.gcloudDatastore = function() {
  var datastore = require('@google-cloud/datastore');
  return datastore;
}

exports.configuredDatastoreImpl = function(projectId, credentials) {
  return require('@google-cloud/datastore')({
    projectId: projectId,
    credentials: credentials
  });
};

exports.keyImpl = function(datastore, key) {
  return datastore.key(key);
};

exports.getImplNoOptions = function(datastore, key) {
  return datastore.get(key);
};

exports.getImplWithOptions = function(datastore, key, options) {
  return datastore.get(key, options);
};

exports.saveImpl = function(datastore, entities) {
  return datastore.save(entities);
};

exports.allocateIdsImpl = function(datastore, ik, n) {
  return datastore.allocateIds(ik, n);
};

exports.handleAllocateImpl = function(allocateResult) {
  return {
    "keys": allocateResult[0],
    "response": allocateResult[1]
  }
}

// TODO:
// createQuery
// createReadStream
// delete
// double
// geoPoint
// int
// runQuery
// runQueryStream
