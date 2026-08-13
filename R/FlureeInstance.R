
#' Class providing objects with methods to interact with a Fluree instance.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON fromJSON
#' @import flureeCrypto
#' @importFrom httr POST
#'
#' @export

FlureeInstance <-  R6::R6Class("FlureeInstance",
  public = list(
    #' @field config (`list()`)\cr
    #' Configuration parameters of the instance.
    config = NULL,

    #' @field connected (`logical`)\cr
    #' Indicates whether connection to the Fluree instance has been established.
    connected = FALSE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param config (`list()`)\cr
    #'   The configuration parameters for the Fluree instance.
    initialize = function(config = list()) {
      privateKey <- config$privateKey
      self$checkConfig(config)
      # Ledger references are branch-qualified, and a config assembled by hand
      # rather than by setConfig() carries no branch. Default it here so every
      # downstream flureeLedgerRef() has one, rather than erroring on connect.
      if (is.null(config$branch)) config$branch <- "main"
      self$config <- config
      if (!is.null(privateKey)) {
        self$setKey(privateKey)
      }
      self$connected <- FALSE
    },

    #' @description
    #' This method validates the configuration parameters and stops with an error
    #' message in the case of invalid parameters.
    #'
    #' @param config (`list()`)\cr
    #'   The configuration parameters for the Fluree instance.
    #' @param isConnecting (`logical`)\cr
    #'   This value indicates whether or not the FlureeInstance is attempting to connect to the host.
    checkConfig = function(config, isConnecting = FALSE) {
      isFlureeHosted <- config$isFlureeHosted
      create <- config$create
      host <- config$host
      port <- config$port
      ledger <- config$ledger
      signMessages <- config$signMessages
      privateKey <- config$privateKey
      apiKey <- config$apiKey

        if (isConnecting) {
          if (!is.null(isFlureeHosted) && isFlureeHosted) {
            if (!is.null(create) && create) {
              stop("Cannot create a ledger through the Fluree hosted service API", call. = FALSE)
            }
          } else {
            # baseUrl is an alternative to host/port, not an addition to it:
            # generateFetchParams() prefers it and derives one from the other.
            if (is.null(host) && is.null(config$baseUrl)) {
              stop("Either `host` or `baseUrl` is required on FlureeInstance or connect",
                   call. = FALSE)
            }
          }
          if (is.null(ledger)) {
            stop("Ledger is required on either FlureeInstance or connect", call. = FALSE)
          }
        }

        if (isTRUE(signMessages) && is.null(privateKey)) {
          stop("Private key is required when signMessages is TRUE", call. = FALSE)
        }

        if (isTRUE(isFlureeHosted)) {
          if (!is.null(host)) {
            stop("Host should not be set when using the Fluree hosted service")
          }
          if (!is.null(port)) {
            stop("Port should not be set when using the Fluree hosted service")
          }
          if (is.null(apiKey) && is.null(privateKey)) {
            stop("Either an apiKey or a privateKey is required for signing messages when using the Fluree hosted service")
          }
        }
    },

    #' @description
    #' Update the configuration parameters.
    #' The new configuration will be merged with the existing one.
    #' The updated configuration parameters are then used by the Fluree instance
    #' for transactions to follow.
    #'
    #' @param newConfig (`list()`)\cr
    #'   The new configuration parameters to be merged with the existing set.
    #' @return [FlureeInstance].
    configure = function(newConfig = list()) {
      mergedConfig <- modifyList(self$config, newConfig)
      if (!is.null(newConfig$defaultContext) && !is.null(self$config$defaultContext)) {
        mergedConfig$defaultContext <- mergeContexts(self$config$defaultContext, newConfig$defaultContext)
      }

      self$checkConfig(mergedConfig)
      self$config <- mergedConfig
      return(self)
    },

    #' @description
    #' Tests the connection by running a minimal query against the ledger.
    #'
    #' @return The query result (a single subject IRI), or an error if the connection fails.
    testLedgers = function() {
      qry <- list(
        select = list("?s"),
        where  = list(list("@id" = "?s", "?p" = "?o")),
        limit  = 1L
      )
      queryInstance <- self$query(qry)
      return(queryInstance$send())
    },

    #' @description
    #' This will test the connection to the host and create the ledger if needed.
    #' The Fluree instance must be 'connected' before querying or transacting.
    #'
    #' @return [FlureeInstance].
    connect = function() {
      self$checkConfig(self$config, TRUE)
      tryCatch({
        fluree_health_check(self$config)
        if (isTRUE(self$config$create)) {
          self$create()
        } else {
          fluree_ledger_info(self$config)
        }
        self$connected <- TRUE
      }, error = function(err) {
        self$connected <- FALSE
        stop(err)
      })
      return(self)
    },

    #' @description
    #' Create a new ledger on the Fluree instance.
    #' If the ledger already exists an error message will be displayed.
    #' The returned Fluree instance will be configured to use the new ledger
    #' for future transactions or queries.
    #'
    #' @param ledgerName (`string`)\cr
    #'   The name of the new ledger to be created.
    #' @param transaction (`list()`)\cr
    #'   The list representation of a transaction to be entered into the
    #'   new ledger (optional).
    create = function(ledgerName = NULL, transaction = NULL) {
      createLedger(self$config, ledgerName = ledgerName,
                   transaction = transaction)
    },

    #' @description
    #' List all branches of the configured ledger.
    #' @return A list of Fluree branch records.
    listBranches = function() {
      listBranches(self$config)
    },

    #' @description
    #' Test whether a branch exists in the configured ledger.
    #' @param branch (`character`)
    #'   Branch name.
    #' @return A single logical value.
    branchExists = function(branch) {
      branchExists(self$config, branch)
    },

    #' @description
    #' Create a branch in the configured ledger.
    #' @param branch (`character`)
    #'   New branch name.
    #' @param from (`character`)
    #'   Source branch. Defaults to the configured branch.
    #' @return The Fluree branch record or an idempotent existing-branch result.
    createBranch = function(branch, from = self$config$branch) {
      createBranch(self$config, branch = branch, from = from)
    },

    #' @description
    #' Upsert JSON-LD resources into a user-defined named graph.
    #' @param document (`list`)
    #'   JSON-LD document, resource, or list of resources.
    #' @param graph (`character`)
    #'   Absolute named-graph IRI.
    #' @param branch (`character`)
    #'   Target branch. Defaults to the configured branch.
    #' @return The parsed Fluree transaction receipt.
    upsertNamedGraph = function(document, graph,
                                branch = self$config$branch) {
      upsertNamedGraph(
        document, graph, self$config, branch = branch
      )
    },

    #' @description
    #' Query a user-defined named graph.
    #' @param query (`list`)
    #'   JSON-LD query.
    #' @param graph (`character`)
    #'   Absolute named-graph IRI.
    #' @param branch (`character`)
    #'   Source branch. Defaults to the configured branch.
    #' @return Parsed query results.
    queryNamedGraph = function(query, graph,
                               branch = self$config$branch) {
      queryNamedGraph(
        query, graph, self$config, branch = branch
      )
    },

    #' @description
    #' Store vector-bearing JSON-LD resources in a named graph.
    #' @param records (`list`)
    #'   JSON-LD resources containing raw numeric embeddings.
    #' @param graph (`character`)
    #'   Absolute named-graph IRI.
    #' @param vector_property (`character`)
    #'   Absolute embedding property IRI.
    #' @param branch (`character`)
    #'   Target branch.
    #' @param ... Additional arguments passed to [upsertVectors()].
    #' @return The parsed Fluree transaction receipt.
    upsertVectors = function(records, graph, vector_property,
                             branch = self$config$branch, ...) {
      upsertVectors(
        records, graph, vector_property, self$config,
        branch = branch, ...
      )
    },

    #' @description
    #' Search vectors in a named graph using exact similarity.
    #' @param graph (`character`)
    #'   Absolute named-graph IRI.
    #' @param vector_property (`character`)
    #'   Absolute embedding property IRI.
    #' @param query_vector (`numeric`)
    #'   Query embedding.
    #' @param branch (`character`)
    #'   Source branch.
    #' @param ... Additional arguments passed to [searchVectors()].
    #' @return Parsed similarity results.
    searchVectors = function(graph, vector_property, query_vector,
                             branch = self$config$branch, ...) {
      searchVectors(
        graph, vector_property, query_vector, self$config,
        branch = branch, ...
      )
    },

    #' @description
    #' Create a new instance of the QueryInstance class.
    #'
    #' @param query (`list()`)\cr
    #'   Representation of the JSON-LD query to perform on the active Fluree instance.
    #' @return [QueryInstance].
    query = function(query) {
      if (!self$connected) {
        stop("You must connect before querying. Try using $connect()$query() instead", call. = FALSE)
      }
      return(QueryInstance$new(query, self$config))
    },

    #' @description
    #' Create a new QueryInstance for a raw SPARQL 1.1 query.
    #'
    #' @param sparql (`character`)\cr
    #'   A SPARQL 1.1 query string.
    #' @param reasoning (`character`)\cr
    #'   Optional reasoning mode: 'rdfs', 'owl2ql', 'owl2rl', 'datalog', or 'none'.
    #' @return [QueryInstance].
    sparql = function(sparql, reasoning = NULL) {
      if (!self$connected) {
        stop("You must connect before querying. Try using $connect()$sparql() instead", call. = FALSE)
      }
      return(QueryInstance$new(sparql, self$config))
    },

    #' @description
    #' Create a new TransactionInstance to insert data into the Fluree ledger.
    #' Insert adds new triples without touching existing ones.
    #'
    #' @param transaction (`list()`)\cr
    #'   A list with a `@graph` key containing the data to insert.
    #' @return [TransactionInstance].
    insert = function(transaction) {
      if (!self$connected) {
        stop("You must connect before transacting. Try using $connect()$insert() instead", call. = FALSE)
      }
      return(TransactionInstance$new(transaction, self$config, endpoint = 'insert'))
    },

    #' @description
    #' Alias for `insert()` for backward compatibility.
    #' @param transaction (`list()`)\cr
    #'   A list with a `@graph` key containing the data to insert.
    #' @return [TransactionInstance].
    transact = function(transaction) {
      self$insert(transaction)
    },

    #' @description
    #' Create a new TransactionInstance to upsert data into the Fluree ledger.
    #' Upsert replaces values for the predicates supplied on each subject;
    #' predicates not mentioned are left untouched.
    #'
    #' @param transaction (`list()`)\cr
    #'   A list with a `@graph` key containing the subjects and properties to upsert.
    #' @return [TransactionInstance].
    upsert = function(transaction) {
      if (!self$connected) {
        stop("You must connect before transacting. Try using $connect()$upsert() instead", call. = FALSE)
      }
      return(TransactionInstance$new(transaction, self$config, endpoint = 'upsert'))
    },

    #' @description
    #' Create a new TransactionInstance for a conditional update (where/delete/insert).
    #' Use when you need to bind current values before replacing them.
    #'
    #' @param transaction (`list()`)\cr
    #'   A list with `where`, `delete`, and optionally `insert` keys.
    #' @return [TransactionInstance].
    update = function(transaction) {
      if (!self$connected) {
        stop("You must connect before transacting. Try using $connect()$update() instead", call. = FALSE)
      }
      return(TransactionInstance$new(transaction, self$config, endpoint = 'update'))
    },

    #' @description
    #' Delete all triples for the given subject IRI(s).
    #'
    #' @param id (`character`)\cr
    #'   One or more subject IRIs to retract.
    #' @return [TransactionInstance].
    delete = function(id) {
      if (!self$connected) {
        stop("You must connect before transacting. Try using $connect()$delete() instead", call. = FALSE)
      }
      idAlias <- findIdAlias(self$config$defaultContext)
      resultingTransaction <- handleDelete(id, idAlias)
      return(TransactionInstance$new(transaction = resultingTransaction, config = self$config, endpoint = 'update'))
    },

    #' @description
    #' Fetch the commit log for the configured ledger.
    #' Returns a summary of each commit: t-value, commit ID, timestamp, assert/retract counts.
    #'
    #' @param query (`list()`)\cr
    #'   Optional parameters: `limit` (integer), `from-t` (start t value), `to-t` (end t value).
    #' @return [HistoryQueryInstance].
    history = function(query = list()) {
      if (!self$connected) {
        stop("You must connect before querying history. Try using $connect()$history() instead", call. = FALSE)
      }
      return(HistoryQueryInstance$new(query, self$config))
    },


    #' @description
    #' Add a private key to the Fluree instance.
    #' This key will be added to the config of the current instance and  will also
    #' be used to sign messages by default when using the `sign()` method on any
    #' future queries or transactions.
    #' The public key and DID will be derived from this private key and added to
    #' the config of the current Fluree instance as well.
    #'
    #' @param privateKey (`string`)\cr
    #'   The private key to use for message signing (represented as a hex string).
    #' @return [FlureeInstance].
    setKey = function(privateKey) {
      publicKey <- flureeCrypto::public_key_from_private(privateKey)
      accountId <- flureeCrypto::account_id_from_public(publicKey)
      did <- sprintf('did:fluree:%s', accountId)
      self$configure(list('privateKey' = privateKey, 'publicKey' = publicKey, 'did' = did))
      return(self)
    },

    #' @description
    #' Generate a new key pair. This method makes use of the flureeCrypto package
    #' to generate a new private key.
    #' The public key and DID are then derived from the private key and these are added
    #' to the config of the current Fluree instance.
    #'
    #' @return [FlureeInstance].
    #'
    generateKeyPair = function() {
      kp <- flureeCrypto::generate_keypair()
      privateKey <- kp[[1]]
      publicKey <- kp[[2]]
      accountId <- flureeCrypto::account_id_from_public(publicKey)
      did <- paste0('did:fluree:', accountId)
      self$configure(list(privateKey = privateKey, publicKey = publicKey, did = did))
      return(self)
    },


    #' @description
    #' Get the private key of the FlureeInstance (if one has been set).
    #'
    #' @return (`string`) | (`undefined`).
    getPrivateKey = function() {
      return(self$config$privateKey)
    },

    #' @description
    #' Get the public key of the FlureeInstance (if one has been set).
    #'
    #' @returns (`string`) | (`undefined`).
    getPublicKey = function() {
      return(self$config$publicKey)
    },

    #' @description
    #' Get the DID of the FlureeInstance (if one has been set).
    #'
    #' @returns (`string`) | (`undefined`).
    getDid = function() {
      return(self$config$did)
    },

    #' @description
    #' The default context set here will be used for all queries and transactions.
    #' Unlike `addToContext()` this method does not merge new context elements
    #' with existing ones, instead it will replace the existing
    #' `defaultContext` entirely.
    #'
    #' @param context (`list()`)\cr
    #'   A named list of JSON-LD prefixes to use as the default context.
    #' @return [FlureeInstance].
    setContext = function(context) {
      self$configure(list(defaultContext = context))
      return(self)
    },

    #' @description
    #' The context set here will be merged with the existing `defaultContext`
    #' and the new merged context will be used for all future queries and
    #' transactions by default.
    #'
    #' @param context (`list()`)\cr
    #'   The context to add to the default for the FlureeInstance.
    #' @return [FlureeInstance].
    addToContext = function(context) {
      if (!is.null(self$config$defaultContext)) {
        newContext <- mergeContexts(self$config$defaultContext, context)
        self$config$defaultContext = newContext
      } else {
        self$config$defaultContext = context
      }
      return(self)
    },

    #' @description
    #' Returns the default context of the FlureeInstance (if it has been set).
    #'
    #' @return (`list()`).
    getContext = function() {
      return(self$config$defaultContext)
    }
  )
)
