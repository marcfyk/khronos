# Khronos

## Overview

Khronos is a CLI tool for calculating timestamps.

## Time Formats

In many of khronos' commands, it will be common use case to format time.


Khronos supports three ways of formatting time:
- UNIX/POSIX (`-u | --unix`)
- ISO8601 (`--iso | --iso8601`)
- Custom string formatting (`-f | --format`)
    - example: "%Y-%m-%d %H:%M:%S"

## Commands

Khronos has a top level help command `--help` / `-h` to display the command options.
Each command also has its respective help command `--help` / `-h` to display information
about the command and its arguments / flags / options that it takes.

- env
- now
- elapse
- range
- fmt

### env

This commands prints the used config for the CLI,
along with where the config file is read from (if any).

### now

This command prints the current time in a specified format.

### elapse

This command prints the elapsed time from now with a given offset.

### range

This command prints a list of new line separated timestamps of given size,
starting from an initial timestamp, and subsequently applying an offset to the
previous timestamp in the list.

### fmt

This command formats a timestamp to a specified format.

## Settings

Khronos does not require any config file and has a set of preset configurations.

However, if you wish to manage and edit configurations, you can add a JSON file
to one of these paths: `$HOME/.khronos.json`, `$HOME/.config/khronos/khronos.json`.
Khronos will look for the a config file in this order and load the first file it finds.

### Defaults

```json
{
    "default_format": "unix",
    "unix": {
        "precision": "ms"
    }
}
```

You do not need to specifiy every field in the configuration. Missing fields will be set to the
preset default values, with the configs settings in overriding where specified in the config file.
