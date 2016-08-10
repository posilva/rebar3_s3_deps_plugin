rebar3-s3-deps
=====

A rebar3 plugin to fetch dependencies with from s3 bucket

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_s3_deps, ".*", {git, "git@host:posilva/rebar3_s3_deps.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_s3_deps
