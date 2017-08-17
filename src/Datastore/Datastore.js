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

// TODO:
// allocateIds
// createQuery
// createReadStream
// delete
// double
// geoPoint
// int
// runQuery
// runQueryStream
