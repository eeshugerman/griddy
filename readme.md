# intro/implementation
Griddy is a (vehicular) traffic simulation/animation library. The initial
conditions are specified with two scheme procedures: `make-skeleton`, which
builds the road network, and `add-actors!`, which does what it says (to the road
network). See `examples/`.

Actors have a location and an agenda. A location is the road component they are
on (typically a lane), and a relative position parameter. An agenda is a list of
tasks, namely `(travel-to <dest>)` or `(sleep-for <time>)`.

Griddy is deterministic; it sidesteps the "simultaneous events problem" by
1. eschewing any stateful devices in modeling actor interaction (in other words,
   there are no slots or queues or the like between actors)
1. building a new data structure for each iteration state instead of mutating a
   single data structure.

To accomplish (2), a fresh road network is generated for each iteration by
calling the `make-skeleton`. The next step is to place actors on the new
network, and to do this we need some way to match up road components between the
previous and next network, and that way, in this case, is a hack: the order in
which road components are registered (`add!`ed) in `make-skeleton` is tracked.
This approach is nice because it's easy/simple, but it has some drawbacks:
- `make-skeleton` must be deterministic within the scope of the simulation
- building a fresh road network every iteration is maybe not super performant
  - using order-registered to match road components means this can't be
    parallelized (not that anything else is parallelized currently... but it
    _could_ be!)


https://user-images.githubusercontent.com/25518211/138569989-890cc568-396b-4851-a06d-2189aca5ecc9.mp4


# getting started
## dev environment setup
- install guix
- install direnv (optional)
### create a guix profile and install dependencies to it
```shell
GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles  # typical yet arbitrary value. may need to be created.
GUIX_PROFILE=$GUIX_EXTRA_PROFILES/griddy
guix pull --profile=$GUIX_PROFILE --channels=channels-spec.scm
guix environment # is this necessary? should it be?
guix package --profile=$GUIX_PROFILE --manifest=manifest.scm
```
### activate the environment
#### with direnv
```shell
cd griddy
direnv allow # only needed first time, when prompted
```
#### without direnv
```shell
cd griddy
source $GUIX_PROFILE/etc/profile
export GUILE_LOAD_PATH=$GUILE_LOAD_PATH:$(pwd)
```

## running a simulation
```shell
guile examples/procedural-grid.scm
```
