# Khronos

## Overview

Khronos is a CLI tool for calculating timestamps.

## Commands

Khronos has a top level help command `--help` / `-h` to display the command options.
Each command also has its respective help command `--help` / `-h` to display information
about the command and its arguments / flags / options that it takes.

- now
- elapse
- range

### now

This command prints the current time in a specified format.

### elapse

This command prints the elapsed time from now with a given offset.

### range

This command prints a list of new line separated timestamps of given size,
starting from an initial timestamp, and subsequently applying an offset to the
previous timestamp in the list.
