#lang typed/racket/base

(require/typed/provide racket/stream
  [stream-length (-> (Sequenceof String) Index)]
  [stream->list (-> (Sequenceof String) (Listof String))]
  [stream-filter (-> (-> String Boolean) (Sequenceof String) (Sequenceof String))]
  [stream-map (-> (-> Index String) (Sequenceof Index) (Sequenceof String))]
  [stream-append (-> (Sequenceof String) * (Sequenceof String))]
)

