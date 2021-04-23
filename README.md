erlang-dkg
=====

[![CI](https://github.com/helium/erlang-dkg/actions/workflows/ci.yml/badge.svg)](https://github.com/helium/erlang-dkg/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/helium/erlang-dkg/branch/master/graph/badge.svg)](https://codecov.io/gh/helium/erlang-dkg)

An implementation of the protocol described in "Distributed Key
Generation in the Wild" (see references) for Erlang (using pairing
based cryptography).  This library will allow some number of Erlang
processes (where N >= 4) to generate one PBC (see
[here](https://github.com/helium/erlang-pbc) for more details) key in
a manner tolerant of Byzantine faults.

Some limitations, where this code does not implement the full protocol:
 - It produces only one key, as it does not support round changes.
 - It does not implement the pessimistic phase (leader change).  Recovery (or rather retry) is handled outside of the protocol.
 - It does not implement the recovery phase.  Since there is no concept of rounds, a user must start a new dkg if the process fails or times out.
 
This code is not run directly, but as a
[relcast](https://github.com/helium/relcast) behavior. To see an example of how this code is run and used, see
[here](https://github.com/helium/miner/blob/master/src/handlers/miner_dkg_handler.erl).
 
Build
-----

    $ make

Test
-----

    $ make test

References
-----

* [Distributed Key Generation in the Wild](https://eprint.iacr.org/2012/377.pdf)
