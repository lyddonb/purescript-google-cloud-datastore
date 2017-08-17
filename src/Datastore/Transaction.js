"use strict";

exports.transactionImpl = function(datastore) {
  return datastore.transaction();
};

exports.transactionSaveImpl = function(transaction, entities) {
  return transaction.save(entities);
};

exports.transactionGetImplNoOptions = function(transaction, key) {
  return transaction.get(key);
};

exports.transactionGetImplWithOptions = function(transaction, key, options) {
  return transaction.get(key, options);
};

exports.transactionRunImpl = function(transaction) {
  return transaction.run();
};

exports.transactionCommitImpl = function(transaction) {
  return transaction.commit();
};

// TODO:
// allocateIds 
// createQuery 
// createReadStream
// delete
// rollback
// runQuery
// runQueryStream
