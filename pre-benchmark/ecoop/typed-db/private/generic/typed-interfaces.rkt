#lang typed/racket
(require/typed ffi/unsafe [#:opaque CPointer cpointer?])
(require/typed/provide "interfaces.rkt"
                       [connection? (Any -> Boolean)]
                       [dbsystem? (Any -> Boolean)]
                       [prepared-statement? ((U String statement-binding (Instance Prepared-Statement<%>)) -> Boolean)]
                       [#:struct statement-binding
                                 ([pst : (Instance Prepared-Statement<%>)]
                                 [params : (Listof Any)])]
                       ; Handle serializable structs opaquely
                       [#:opaque Simple-Result simple-result?]
                       [simple-result-info (Simple-Result -> FieldInfo)]
                       [#:opaque Rows-Result rows-result?]
                       [rows-result-headers (Rows-Result -> Header)]
                       [rows-result-rows (Rows-Result -> (Listof (Vectorof Any)))]
                       [#:struct (exn:fail:sql exn:fail)
                                 ([sqlstate : Any]
                                  [info : String])]
                       [error/internal (Symbol String Any * -> Nothing)]
                       [error/internal* (Symbol String Any * -> Nothing)]
                       [error/not-connected (Symbol -> Nothing)]
                       [error/no-support (Symbol Any -> Nothing)]
                       [error/need-password (Symbol -> Nothing)]
                       [error/comm (case-> (Symbol -> Nothing) (Symbol Boolean -> Nothing))]
                       [error/hopeless (Symbol -> Nothing)]
                       [error/unsupported-type (case-> (Symbol Any -> Nothing) (Symbol Any Any -> Nothing))]
                       [error/no-convert (case-> (Symbol Any Any Any [#:contract Any] -> Nothing)
                                                 (Symbol Any Any Any Any [#:contract Any] -> Nothing)   )]
                       [error/invalid-nested-isolation (Symbol Any -> Nothing)]
                       [error/tx-bad-stmt (Symbol Any Any -> Nothing)]
                       [error/unbalanced-tx (Symbol Any Boolean -> Nothing)]
                       [error/unclosed-tx (Symbol Any Boolean -> Nothing)]
                       [error/nested-tx-option (Symbol Any -> Nothing)]
                       [error/exn-in-rollback (Symbol exn exn -> Nothing)]
                       [error/stmt-arity (Symbol Any Any -> Nothing)]
                       [error/stmt (Symbol Any String Any * -> Nothing)] ; these types are wrong, but can't specify statically that the rest args come in pairs
                       [error/want-rows (Symbol Any Boolean -> Nothing)]
                       [error/want-cursor (Symbol Any -> Nothing)]
                       [error/column-count (Symbol Any Any Any Boolean -> Nothing)]
                       [error/row-count (Symbol Any Any Any -> Any)]
                       [error/statement-binding-args (Symbol Any Any -> Nothing)])
(provide ParameterHandler TypeDesc FieldInfo Header Connection<%> DBSystem<%> Prepared-Statement<%>)
(define-type ParameterHandler (Symbol Index Any -> Any))
(define-type TypeDesc (List Boolean (Option Symbol) Any))
(define-type FieldInfo (Listof (Pairof Any Any)))
(define-type Header (Listof FieldInfo))
(define-type Connection<%>
  (Class [disconnect (-> Void)]
         [get-dbsystem (-> (Instance DBSystem<%>))]
         [query (Symbol Any Any -> Any)] ; What are statement and QueryResult
         [prepare (Symbol Any Boolean -> (Instance Prepared-Statement<%>))]
         [fetch/cursor (Symbol Any Integer -> (Option (Listof (Vectorof Any))))]
         [get-base (-> (Option (Instance Connection<%>)))]
         [start-transaction (Symbol Symbol Any Boolean -> Void)]
         [end-transaction (Symbol (U 'commit 'rollback) Boolean -> Void)]
         [transaction-status (Symbol -> (U Boolean 'invalid))]
         [free-statement ((Instance Prepared-Statement<%>) Boolean -> Void)]))
(define-type DBSystem<%>
  (Class [get-short-name (-> Symbol)]
         [get-parameter-handlers ((Listof Any) -> (Listof ParameterHandler))]
         [field-dvecs->typeids ((Listof Any) -> (Listof Any))]
         [get-known-types (Real -> (Listof Symbol))]
         [describe-params ((Listof Any) -> (Listof TypeDesc))]
         [describe-fields ((Listof Any) -> (Listof TypeDesc))]))
(define-type Prepared-Statement<%>
  (Class [get-handle (-> CPointer)]
         [set-handle (CPointer -> Void)]
         [get-stmt (-> (Option String))]
         [get-stmt-type (-> (Option Symbol))]
         [get-param-count (-> Integer)]
         [get-param-typeids (-> (Listof Any))]
         [get-result-dvecs (-> (Listof Any))]
         [get-result-count (-> Integer)]
         [get-result-typeids (-> (Listof Any))]
         [check-owner (Symbol (Instance Connection<%>) Any -> Void)]
         [bind (Symbol (Listof Any) -> statement-binding)]
         [get-param-types (-> (Listof TypeDesc))]
         [get-result-types (-> (Listof TypeDesc))]))
