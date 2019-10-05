#!/bin/bash

set -eu

trap 'chown $(stat -c "%u:%g" Makefile) -R tmp out' EXIT
make all utils test
