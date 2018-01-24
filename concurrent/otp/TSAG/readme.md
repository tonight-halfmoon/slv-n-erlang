
# TSAG

A Riak TS client for [GenRS](../genrs) to communicate with Riak TS 

## Riak Erlang Client Library

[Riak Erlang Client library](https://github.com/basho/riak-erlang-client) provides an API against Riak.

## Configuration

```
Straight forward: clone and make.
```

### Environment variable

> First option

Set environment variable PATH_TO_RIAKC as suggested. For example, on archlinux it is done with [Environment Variables on Archlinux](https://github.com/tonight-halfmoon/shabang/tree/master/archlinux/environment.variables).

> Second option

Add to environment variable $ERL_LIBS both RIAKC and RIAKC_DEPS_HAMCREST and RIAKC_DEPS_PB as shown in (ERL_LIBS 2)[https://github.com/tonight-halfmoon/shabang/tree/master/archlinux/environment.variables/etc_profile_d_erl_libs_2_sh].

## Development

If you set the environment variable according to the first option, then:

   erl -pa $PATH_TO_RIAKC/ebin $PATH_TO_RIAKC/deps/*/ebin ebin/

Otherwise, if all reltated environment variables have been set according to the second option then we do not need to pass them explicitely.
