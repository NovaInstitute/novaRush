
#' The main class for interacting with Fluree
#' 
#' @param config [list] A list of configuration parameters for the instance
#' @param config$ledger [string] The name of the ledger to be created or transacted to
#' @param config$host [string] The host where the instance is running (e.g. 'localhost')
#' @param config$port [string] The port where the instance is running (e.g. '58090')
#' @param config$create [boolean] If true a new ledger (with the specified name above) will be created if it does not exist already
#' @param config$privateKey [string] The private key to be used for message signing
#' @param config$signMessages [boolean]  If true messages will be signed automatically
#' @param config$defaultContext [string] The default context to be used for queries and transactions
#' @param config$isFlureeHosted [boolean] If true the Fluree hosted service will be used
#' @param config$apiKey [string] The API key to be used by the Fluree hosted service
#' 
#' @importFrom R6 R6Class 
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @import flureeCrypto
#' 
#' @examples
#' test <- FlureeInstance$new(list(
#'    host = 'localhost', 
#'    port = '58090', 
#'    ledger = 'newLedger', 
#'    signMessages = TRUE, 
#'    privateKey = '913524961748600e1a7fd57e8724d2c3ddaa5b5377e0985e873c7f5294a480d1', 
#'    create = TRUE)
#'  )$connect()
#'
#'  test$transact(
#'  )$send()
#'
FlureeInstance <- R6Class(
  "FlureeInstance",
  public = list(
    config = NULL,
    connected = FALSE,
    
    # Constructor method
    initialize = function(config = list()) {
      privateKey <- config$privateKey
      self$checkConfig(config)
      self$config <- config
      if (!is.null(privateKey)) {
        self$setKey(config$privateKey)
      }
      self$connected <- FALSE
    },
    
    # Method to validate configuration parameters
    checkConfig = function(config, isConnecting = FALSE) {
      
      isFlureeHosted <- config$isFlureeHosted
      create <- config$create
      host <- config$host
      ledger <- config$ledger
      signMessages <- config$signMessages
      privateKey <- config$privateKey
      apiKey <- config$apiKey
      
      with(config, {
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
        
        if (!is.null(signMessages) && signMessages && is.null(privateKey)) {
          stop("Private key is required when signMessages is TRUE", call. = FALSE)
        }
        
        if (!is.null(isFlureeHosted) && isFlureeHosted) {
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
      })
    },
    
    #' Update the configuration parameters of the Fluree instance
    #' 
    #' @description
    #' The new configuration will be merged with the existing one.
    #' The updated configuration parameters are then used by the Fluree instance for transactions to follow.
    #' 
    #' @param newConfig [list] The new configuration object
    #' @param newConfig$ledger [string] The name of the ledger to be created or transacted to
    #' @param newConfig$host [string] The host where the instance is running (e.g. 'localhost')
    #' @param newConfig$port [string] The port where the instance is running (e.g. '58090')
    #' @param newConfig$create [boolean] If true a new ledger (with the specified name above) will be created if it does not exist already
    #' @param newConfig$privateKey [string] The private key to be used for message signing
    #' @param newConfig$signMessages [boolean]  If true messages will be signed automatically
    #' @param newConfig$defaultContext [string] The default context to be used for queries and transactions
    #' @param newConfig$isFlureeHosted [boolean] If true the Fluree hosted service will be used
    #' @param newConfig$apiKey [string] The API key to be used by the Fluree hosted service
    #' 
    #' @returns FlureeInstance (with updated config)
    #' 
    #' @examples
    #' test <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger')
    #'    )$connect()
    #' 
    #' updatedInstance <- test$config(list(
    #'    privateKey = '913524961748600e1a7fd57e8724d2c3ddaa5b5377e0985e873c7f5294a480d1',
    #'    signMessages = TRUE)
    #'    )
    #' 
    configure = function(newConfig = list()) {
      mergedConfig <- modifyList(self$config, newConfig)
      if (!is.null(newConfig$defaultContext) && !is.null(self$config$defaultContext)) {
        mergedConfig$defaultContext <- mergeContexts(self$config$defaultContext, newConfig$defaultContext)
      }
      
      self$checkConfig(mergedConfig)
      self$config <- mergedConfig
      return(self)
    },
    
# TODO:  testLedgers() function
    
    #' Connect to the Fluree instance
    #' 
    #' @description
    #' This will test the connection and create the ledger if needed.
    #' The Fluree instance must be connected before querying or transacting.
    #' 
    #' @returns FlureeInstance
    #' 
    #' @examples
    #' test <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger', 
    #'    signMessages = TRUE, 
    #'    privateKey = '913524961748600e1a7fd57e8724d2c3ddaa5b5377e0985e873c7f5294a480d1', 
    #'    create = TRUE)
    #'  )$connect()
    #' 
    connect = function() {
      self$checkConfig(self$config, TRUE)
      self$connected <- TRUE
      
      tryCatch({
        if (isTRUE(self$config$create)) {
          self$create()
        }
#TODO:  #self$testLedger()
        
      }, error = function(err) {
        self$connected <- FALSE
        stop(err)
      })
      return(self)
    },
    
    #' Create a new ledger on the Fluree instance
    #' 
    #' @description
    #' If the ledger already exists an error message will be displayed. 
    #' The returned Fluree instance will be configured to use the new ledger for future transactions or queries.
    #' 
    #' @param ledgerName [string] The name of the new ledger to be created
    #' @param transaction [list] An optional transaction to be included in the new ledger
    #' 
    #' @returns FlureeInstance
    #' 
    #' @examples
    #' test <- FlureeInstance$new(list(
    #'     host = 'localhost',
    #'     port = '58090',
    #'     ledger = 'Example2')
    #'   )
    #' createdInstance <- test$create('newLedger', )
    #' 
    create = function(ledgerName = NULL, transaction = NULL) {
      config <- self$config
      with(config, {
        
        url <- paste0('http://', host)
        if (!is.null(port)) {
          url <- paste(url, sep = ":", port)
        }
        url <- paste0(url, "/fluree/create")
        
        body <- list(
          ledger = ledgerName %||% ledger,
          insert = list(message = "success")
        )
        
        if (!is.null(transaction)) {
          body <- modifyList(body, transaction)
        }

        header = 'application/json'
        finalBody = toString(toJSON(body, auto_unbox = TRUE, pretty = TRUE))

        if (!is.null(signMessages) && signMessages && !is.null(privateKey)) {
          finalBody <- flureeCrypto:::serialize_jws(finalBody, privateKey)
          header = 'application/jwt'
        }
        
        response <- POST(
          url = url,
          add_headers(`Content-Type` = header),
          body = finalBody,
          encode = "raw"
        )
        
        # Output the results
        print(content(response, as = "text"))
      })
    },
    
#TODO:  define QueryInstance class
    query = function(query) {
      if (!self$connected) {
        stop("You must connect before querying. Try using .connect().query() instead", call. = FALSE)
      }
      
      # add 'from' to the query if not already present
      if (!is.null(query$from)) {
        query$from <- self$config$ledger
      }
      return(QueryInstance$new(query, self$config))
    },


    #' Creates a new TransactionInstance
    #' 
    #' @description
    #' This new TransactionInstance can then be used to transact with the Fluree database.
    #' 
    #' @param transaction [list] The transaction to send to the FlureeInstance
    #' 
    #' @returns TransactionInstance
    #' 
    #' @examples
    #' txn <- connectedInstance$transact(list(
    #' insert = list('@id' = 'freddy', 'name' = 'Freddy')))
    #' 
    #' response <- txn$send()
    #' 
    transact = function(transaction) {
      print('In transaction method...')
      if (!self$connected) {
        stop("You must connect before transacting. Try using $connect()$transact() instead", call. = FALSE)
      }
      
      if (is.null(transaction$ledger)) {
        transaction$ledger <- self$config$ledger
      }
      
      return(TransactionInstance$new(transaction, self$config))
    },


# TODO:  handle upsert

# TODO:  handle delete

# TODO:  handle history
    
    #' Add a private key to the FlureeInstance
    #' 
    #' @description
    #' This key will then be added to the config of the current instance.  This key will be used to
    #' sign messages by default when using the sign() method on any future queries or transactions.
    #' The public key and DID will be derived from the private key and added to the config 
    #' of the current FlureeInstance.
    #' 
    #' @seealso [generateKeyPair()]
    #' 
    #' @param privateKey [string] The private key to use for message signing
    #' 
    #' @returns FlureeInstance
    #' 
    #' @examples
    #' connectedInstance <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger')
    #'  )$connect()
    #'  
    #'  addKey <- connectedInstance$setKey('XXXXXXXXXXXXXXXX')
    #'  
    #'  response <- connectedInstance$query(list(
    #'  select = list(
    #'  'freddy' = ["*"])))$sign()$send()
    #' 
    #' 
    setKey = function(privateKey) {
      publicKey <- flureeCrypto::public_key_from_private(privateKey)
      accountId <- flureeCrypto::account_id_from_public(publicKey)
      did <- sprintf('did:fluree:%s', accountId)
      self$configure(list('privateKey' = privateKey, 'publicKey' = publicKey, 'did' = did))
      return(self)
    },
    
    #' Generate a new key pair
    #' 
    #' @description
    #' This method makes use of the flureeCrypto package to generate a new private key.
    #' The public key and DID are then derived from the private key and these are added 
    #' to the config of the current FlureeInstance.
    #' 
    #' @returns FlureeInstance
    #' 
    #' @examples
    #' connectedInstance <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger')
    #'  )
    #'  
    #'  connectedInstance$generateKeyPair()
    #'  
    #'  privateKey <- connectedInstance$getPrivateKey()
    #'  publicKey <- connectedInstance$getPublicKey()
    #'  did <- connectedInstance$getDid()
    #' 
    generateKeyPair = function() {
      kp <- flureeCrypto::generate_keypair()
      privateKey <- kp[[1]]
      publicKey <- kp[[2]]
      accountId <- flureeCrypto::account_id_from_public(publicKey)
      did <- paste0('did:fluree:', accountId)
      self$configure(list(privateKey, publicKey, did))
      return(self)
    },
    
    #' Returns the private key of the FlureeInstance (if one has been set)
    #' 
    #' @returns string | undefined
    #' 
    #' @examples
    #' connectedInstance <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger')
    #'  )
    #' 
    #' connectedInstance$generateKeyPair()
    #' 
    #' privateKey <- connectedInstance$getPrivateKey()
    #' 
    getPrivateKey = function() {
      return(self$config$privateKey)
    },
    
    #' Returns the public key of the FlureeInstance (if one has been set)
    #' 
    #' @returns string | undefined
    #' 
    #' @examples
    #' connectedInstance <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger')
    #'  )
    #' 
    #' connectedInstance$generateKeyPair()
    #' 
    #' publicKey <- connectedInstance$getPublicKey()
    #' 
    getPublicKey = function() {
      return(self$config$publicKey)
    },
    
    #' Returns the DID of the FlureeInstance (if one has been set)
    #' 
    #' @returns string | undefined
    #' 
    #' @examples
    #' connectedInstance <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger')
    #'  )
    #' 
    #' connectedInstance$generateKeyPair()
    #' 
    #' privateKey <- connectedInstance$getDid()
    #' 
    getDid = function() {
      return(self$config$did)
    },
    
    #' Set the default context of the FlureeInstance
    #' 
    #' @description
    #' The context set here will be used for all queries and transactions.
    #' Unlike addToContext() this method does not merge new context elements with existing ones,
    #' instead it will replace the existing defaultContext entirely.
    #' 
    #' @param context [list] The context to set as the default for the FlureeInstance
    #' 
    #' @returns FlureeInstance
    #' 
    #' @examples
    #' connectedInstance <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger')
    #' )
    #' 
    #' connectedInstance$setContext(list("ex" = "http://example.org/"))
    #' 
    setContext = function(context) {
      self$configure(list(defaultContext = context))
      return(self)
    },
    
    #' Adds to the default context of the FlureeInstance
    #' 
    #' @description
    #' The context set here will be merged with the existing defaultContext and 
    #' the new merged context will be used for all queries and transactions.
    #' 
    #' @seealso [setContext()]
    #' 
    #' @param context [list] The context to add to the default for the FlureeInstance
    #' 
    #' @returns FlureeInstance
    #' 
    #' @examples
    #' connectedInstance <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger')
    #' )
    #' 
    #' connectedInstance$addToContext(list("ex" = "http://example.org/"))
    #' 
    addToContext = function(context) {
      if (!is.null(self$config$defaultContext)) {
        newContext <- mergeContexts(self$config$defaultContext, context)
        self$config$defaultContext = newContext
      } else {
        self$config$defaultContext = context
      }
      return(self)
    },
    
    #' Returns the default context of the FlureeInstance (if is has been set)
    #' 
    #' @param context [list] The context to set as the default for the FlureeInstance
    #' 
    #' @returns list
    #' 
    #' @examples
    #' connectedInstance <- FlureeInstance$new(list(
    #'    host = 'localhost', 
    #'    port = '58090', 
    #'    ledger = 'newLedger',
    #'    defaultContext = list("schema" = "http://schema.org/"))
    #' )
    #' 
    getContext = function() {
      return(self$config$defaultContext)
    }
  )
)
