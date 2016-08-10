-module(rebar3_s3_resource).

-behaviour(rebar_resource).

-export([
  lock/2,
  download/3,
  needs_update/2,
  make_vsn/1
]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
%% ===================================================================
%% Public API
%% ===================================================================
%% TODO: Spec functions
-spec lock(file:filename_all(), tuple()) -> rebar_resource:resource().
lock(_Dir, Source) ->
  Source.
download(Dir, {s3, Bucket, {object, Object}}, State) ->  
  download(Dir, {s3, Bucket, {object, Object}, []}, State);
download(Dir, {s3, Bucket, {object, Object}, Options}, _State) ->
  Config = init_resource(Options),
  ok = filelib:ensure_dir(Dir),
  rebar_api:debug("Downloading object: ~p", [{Bucket, Object}]),
  S3Object = erlcloud_s3:get_object(Bucket, Object, Config),
  Content  = proplists:get_value(content, S3Object),
  FileName = filename:basename(Object), 
  FullName = filename:join([Dir, FileName]),
  {ok, ObjectFD} = file:open(FullName, [write]),
  ok = file:write(ObjectFD, Content),
  ok = file:close(ObjectFD),
  {ok, FullName};
download(Dir, {s3, Bucket, {recursive, Path}}, State) ->  
  download(Dir, {s3, Bucket, {recursive, Path}, []}, State);
download(Dir, {s3, Bucket, {recursive, Path}, Options}, _State) ->
  Config = init_resource(Options),
  ok = filelib:ensure_dir(Dir),

  rebar_api:debug("Downloading recursive Path from: ~p", [{Bucket, Path}]),
  AWSConfig = erlang:get({?MODULE, aws_config}),
  ListOfObjects =  erlcloud_s3:list_objects(Bucket, [{prefix, Path}], AWSConfig),
  rebar_api:debug("S3 objects list: ~p", [ListOfObjects]),
  Content  = proplists:get_value(contents, ListOfObjects),
  OutputDir = filename:join([Dir, Path]),
  lists:foreach(fun(E) -> 
    Key = proplists:get_value(key, E),

    FullName = filename:join([Dir, Key]),
    DirName = filename:dirname(FullName),

    ok = filelib:ensure_dir(FullName),

    rebar_api:debug("Getting object: ~p from bucket ~p, will be saved at ~p", [Key, Bucket, DirName]),
    S3Object = erlcloud_s3:get_object(Bucket, Key, Config),

    FileContent  = proplists:get_value(content, S3Object),
    rebar_api:debug("Saving object: ~p to  ~p", [Key, FullName]),
    {ok, ObjectFD} = file:open(FullName, [write]),

    ok = file:write(ObjectFD, FileContent),

    ok = file:close(ObjectFD)
  end, Content),  
  {ok, OutputDir}.

-spec needs_update(file:filename_all(), tuple()) -> boolean().
needs_update(_Dir, _Source) ->
  true.

-spec make_vsn(file:filename_all()) -> {plain, string()} | {error, string()}.
make_vsn(_Dir) ->
  {plain, "1.2.0"}. %% TODO how to check a version 
  %%{error, "Replacing version of type s3 not supported."}.

%% ===================================================================
%% Internal Functions
%% ===================================================================
init_resource(Options) -> 
  ok = check_options(Options),
  _ = application:ensure_all_started(erlcloud),
  AWSConfig = init_aws_api(Options),
  erlang:put({?MODULE, aws_config}, AWSConfig),
  AWSConfig.

init_aws_api(Options) -> 
  Profile = extract_profile_option(Options),
  Region  = extract_region_option(Options),
  Timeout  = extract_timeout_option(Options),
  {AccessKeyID, SecretAccessKey} = get_aws_credentials(Profile),
  S3Host = get_s3_host(Region),
  AWSConfig = erlcloud_s3:new(AccessKeyID, SecretAccessKey, S3Host),
  AWSConfig#aws_config{timeout = Timeout}.
get_aws_credentials(undefined) -> 
  AWSDefaultConfig = erlcloud_aws:default_config(),
  { AWSDefaultConfig#aws_config.access_key_id, 
    AWSDefaultConfig#aws_config.secret_access_key};
get_aws_credentials(Profile) -> 
  {ok, AWSDefaultConfig} = erlcloud_aws:profile(Profile),
  { AWSDefaultConfig#aws_config.access_key_id, 
    AWSDefaultConfig#aws_config.secret_access_key}.

get_s3_host(undefined) -> 
  AWSDefaultConfig = erlcloud_aws:default_config(),
  AWSDefaultConfig#aws_config.s3_host;
get_s3_host(Region) when is_list(Region) -> 
  "s3-" ++ Region ++ ".amazonaws.com";
get_s3_host(Region) when is_binary(Region) -> 
  get_s3_host(erlang:binary_to_list(Region)).

extract_profile_option(Options) -> 
  proplists:get_value(profile, Options, undefined).
extract_region_option(Options) -> 
  proplists:get_value(region, Options, undefined).
extract_timeout_option(Options) -> 
  proplists:get_value(timeout, Options, infinity).

check_options([]) -> 
  ok;
check_options([{profile, Profile} | Rest]) when Profile =:= default -> 
  check_options(Rest);
check_options([{profile, Profile} | Rest]) when is_list(Profile) ; is_binary(Profile) ; is_atom(Profile)-> 
  check_options(Rest);
check_options([{region, Region} | Rest]) when is_list(Region) ; is_binary(Region) -> 
  check_options(Rest);
check_options([{timeout, Timeout} | Rest]) when is_integer(Timeout), Timeout > 0-> 
  check_options(Rest);  
check_options([Other| _]) -> 
  rebar_api:error("Invalid options: ~p ", [Other]),
  rebar_api:abort().  