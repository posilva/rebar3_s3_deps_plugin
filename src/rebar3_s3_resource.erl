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
  {ok, FullName}.

-spec needs_update(file:filename_all(), tuple()) -> boolean().
needs_update(_Dir, _Source) ->
  true.

-spec make_vsn(file:filename_all()) -> {plain, string()} | {error, string()}.
make_vsn(_Dir) ->
  {plain, "1.2.0"}.
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
  {AccessKeyID, SecretAccessKey} = get_aws_credentials(Profile),
  S3Host = get_s3_host(Region),
  erlcloud_s3:new(AccessKeyID, SecretAccessKey, S3Host).
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

check_options([]) -> 
  ok;
check_options([{profile, Profile} | Rest]) when Profile =:= default -> 
  check_options(Rest);
check_options([{profile, Profile} | Rest]) when is_list(Profile) ; is_binary(Profile) ; is_atom(Profile)-> 
  check_options(Rest);
check_options([{region, Profile} | Rest]) when is_list(Profile) ; is_binary(Profile) -> 
  check_options(Rest);
check_options([Other| _]) -> 
  rebar_api:error("Invalid options: ~p ", [Other]),
  rebar_api:abort().  