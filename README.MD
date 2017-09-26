# ChessClock
Chess clock for playing chess with time control

## version 0.0.1.3

## Synopsis
chessClock [-t] [-i]

## Description
Use this little tool if you want to play chess with time control but have no chess clock handy.

It's CLI software, no GUI.

### Keys
spacebar - make move

### Arguments (Erroneuos input would be ignored and default values would be used, maximum time for the moment is 99:59, everything greater than that will be brought to the maximum)
**-t mm:ss** - set time (default: 10:00)

**-i sec** - set increment (default 0)

## Example usage

```sh

chessClock -t 03:00 -i 2 # time control - 3 minutes with 2 seconds increment
chessClock -i 2 # time control - 10 minutes (default) with 2 seconds increment
chessClock -t 15:00 # time control - 15 minutes with no increment (default)
chessClock # default time control (10 minutes with no increment)

```