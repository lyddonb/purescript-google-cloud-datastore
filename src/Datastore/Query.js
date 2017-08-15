"use strict";

exports.createQueryImplNoNamespace = function(datastore, kind) {
  return datastore.createQuery(kind);
};

exports.createQueryImplWithNamespace = function(datastore, namespace, kind) {
  return datastore.createQuery(namespace, kind);
};

exports.endImpl = function(cursorToken, query) {
  return query.end(cursorToken);
};

exports.startImpl = function(cursorToken, query) {
  return query.start(cursorToken);
};

exports.hasAncestorImpl = function(key, query) {
  return query.hasAncestor(key);
};

exports.limitImpl = function(count, query) {
  return query.limit(count);
};

exports.offsetImpl = function(offset, query) {
  return query.offset(offset);
};

exports.filterImpl = function(prop, operator, value, query) {
  return query.filter(prop, operator, value);
};

exports.groupByImpl = function(props, query) {
  return query.groupBy(props);
};

exports.selectImpl = function(props, query) {
  return query.select(props);
};

exports.orderImpl = function(prop, opts, query) {
  return query.order(prop, opts);
};

exports.runOptionsImpl = function(query, options) {
  return query.run(options);
};

exports.runNoOptionsImpl = function(query) {
  return query.run();
};

// TODO:
// runStream
