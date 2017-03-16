# ChessClock
Chess clock for playing chess with time control

chessClock [-t] [-i]

## version 0.0.1.0

Use this little tool if you have no chess clock handy but want to play chess with time control. Press the spacebar to make move.

It's CLI software, no GUI.

*-t mm:ss - set time (default: 10:00)*

*-i sec - set increment (default 0)*

Example usage

```sh

chessClock -t 03:00 -i 2 # time control - 3 minutes with 2 seconds increment
chessClock -i 2 # time control - 10 minutes (default) with 2 seconds increment
chessClock -t 15:00 # time control - 15 minutes wth no increment (default)
chessClock # default time control (10 minutes with no increment)

```