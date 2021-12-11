# intro
// TODO

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
