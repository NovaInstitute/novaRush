
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
            if (is.null(host)) {
              stop("Host is required on either FlureeInstance or connect", call. = FALSE)
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

#' testLedgers
#'
#' @returns
#' @export
#'
#' @examples
    testLedgers = function() {
      qry <- '{
          "where": {
             "@id": "?s",
             "?p": "?o",
           },
           "select": ["?s"],
           "limit": 1
      }'
      queryInstance = self$query(qry)
      return(queryInstance$send())
    },

    #' @description
    #' This will test the connection to the host and create the ledger if needed.
    #' The Fluree instance must be 'connected' before querying or transacting.
    #'
    #' @return [FlureeInstance].
    connect = function() {
      self$checkConfig(self$config, TRUE)
      self$connected <- TRUE

      tryCatch({
        if (isTRUE(self$config$create)) {
          self$create()
        }
        #self$testLedger()

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
      config <- self$config
      ledger <- config$ledger
      signMessages <- config$signMessages
      privateKey <- config$privateKey

      params <- generateFetchParams(config, 'create')
      url <- params$url

      body <- list(ledger = ledgerName %||% ledger)
      if (!is.null(transaction)) {
        body <- modifyList(body, transaction)
      }

      contentType <- 'application/json'
      finalBody <- do.call(jsonlite::toJSON, c(list(x = body), novaRush:::getDefaultToJSONargs()))

      if (isTRUE(signMessages) && !is.null(privateKey)) {
        finalBody <- flureeCrypto:::serialize_jws(as.character(finalBody), privateKey)
        contentType <- 'application/jwt'
      }

      response <- POST(
        url = url,
        add_headers(`Content-Type` = contentType),
        body = finalBody,
        encode = "raw"
      )

      resp_text <- httr::content(response, as = "text", encoding = "UTF-8")
      if (httr::http_error(response)) {
        stop("Failed to create ledger: ", resp_text)
      }

      do.call(jsonlite::fromJSON, c(list(txt = resp_text), novaRush:::getDefaultFromJSONargs()))
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
      body <- list(query = sparql)
      if (!is.null(reasoning)) {
        body$reasoning <- reasoning
      }
      return(QueryInstance$new(body, self$config, endpoint = 'sparql'))
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
    #' Create a new instance of the HistoryInstance class.
    #' This new HistoryInstance can then be used to transact with the Fluree database.
    #'
    #' @param query (`list()`)\cr
    #'   Representation of the transaction to send to the active Fluree instance.
    #' @return [HistoryQueryInstance].
    history = function(query) {
      if (!self$connected) {
        stop("You must connect before querying history. Try using $connect()$transact() instead", call. = FALSE)
      }

      if (is.null(query$from)) {
        query$from <- self$config$ledger
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
