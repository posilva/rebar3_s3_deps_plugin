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

Add the s3 resource dependency in rebar config like following:

	{appname, {s3, "my-deploy-bucket-name", {object, "prod/v1.0/raw_dep.tgz"}, 
                [
                    {profile, default}, %% The AWS credentials profile   
                    {tar,[compressed]}, %% Decompress the compressed tar file
                    {raw, [{vsn, "1.2.1"}]} %% It's a raw dependency
    			]
    			}
    }

