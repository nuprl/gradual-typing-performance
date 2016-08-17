ssmpler
===

On the cluster,
 check that a few configs give the same results on different nodes.


#### Usage:

  0. Edit `make-config-sampler.rkt` with configurations you want to run
  1. Run `make-config-sampler.rkt`
  2. Compress the configs, move them to the cluster
  3. Run `./config-sampler.sh` on the cluster.
     This will spawn a few nodes that run copies of the compressed configs


#### Summary:

- `make-config-sampler.rkt` Collect datasets 
- `config-sampler.sh`
- `config-sampler-node.sh`
- `config-sampler.tar.gz` generated file, `config-sampler.sh` expects this to be unzipped.
