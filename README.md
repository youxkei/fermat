# Fermat
**Fermat** (/fɛrˈma/, フェルマー) is an opinionated code formatter for Erlang.

Fermat can also remove trailing separators.

## Table of Contents
* [Installation](#installation)
* [Usage](#usage)

## Installation
You can download the binary from the release page.

https://github.com/youxkei/fermat/releases/latest

## Usage

### Format code with removing trailing separators
```console
$ cat <<EOF > /tmp/foo.erl
-module(foo).
-export([f/1,]).

f() when true,; ->
    [1234567890 || X <- [1234567890,], Y <- [1234567890,], foo:is_valid(X,),],;.
EOF

$ fermat -l 30 /tmp/foo.erl
-module(foo).
-export([f/1]).

f() when true ->
    [1234567890
     || X <- [1234567890],
        Y <- [1234567890],
        foo:is_valid(X)].
```

### Don't format but remove trailing separators
```console
$ cat <<EOF > /tmp/foo.erl
-module(foo).
-export([f/1,]).

f() when true,; ->
    [1234567890 || X <- [1234567890,], Y <- [1234567890,], foo:is_valid(X,),],;.
EOF

$ fermat -n /tmp/foo.erl
-module(foo).
-export([f/1]).

f() when true ->
    [1234567890 || X <- [1234567890], Y <- [1234567890], foo:is_valid(X)].
```
