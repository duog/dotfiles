#!/usr/bin/env bash

(
  for i in $(polybar -m | cut -f 1 -s -d :); do
    MONITOR="$i" polybar main &
  done;
  wait
) &
