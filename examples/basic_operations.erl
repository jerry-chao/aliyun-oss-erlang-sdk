%%%-------------------------------------------------------------------
%%% @doc Basic operations example for Aliyun OSS Erlang SDK
%%% @end
%%%-------------------------------------------------------------------
-module(basic_operations).

-export([
    run/0
]).

%% @doc Run the example
run() ->
    % Set your credentials and endpoint
    AccessKeyId = os:getenv("OSS_ACCESS_KEY_ID"),
    AccessKeySecret = os:getenv("OSS_ACCESS_KEY_SECRET"),
    Endpoint = os:getenv("OSS_ENDPOINT"),
    BucketName = os:getenv("OSS_BUCKET_NAME"),
    
    % Create auth and bucket objects
    Auth = aliyun_oss:auth(AccessKeyId, AccessKeySecret),
    Bucket = aliyun_oss:bucket(Auth, Endpoint, BucketName),
    
    % Define a key for our object
    Key = "example.txt",
    Content = "Hello, Aliyun OSS from Erlang!",
    
    % Upload the object
    io:format("Uploading object ~s...~n", [Key]),
    case aliyun_oss:put_object(Bucket, Key, Content) of
        {ok, Result1} ->
            io:format("Upload successful: ~p~n", [Result1]),
            
            % Get the object
            io:format("Getting object ~s...~n", [Key]),
            case aliyun_oss:get_object(Bucket, Key) of
                {ok, #{body := Body} = Result2} ->
                    io:format("Get successful: ~p~n", [Result2]),
                    io:format("Content: ~s~n", [Body]),
                    
                    % Delete the object
                    io:format("Deleting object ~s...~n", [Key]),
                    case aliyun_oss:delete_object(Bucket, Key) of
                        {ok, Result3} ->
                            io:format("Delete successful: ~p~n", [Result3]);
                        {error, Error3} ->
                            io:format("Delete failed: ~p~n", [Error3])
                    end;
                {error, Error2} ->
                    io:format("Get failed: ~p~n", [Error2])
            end;
        {error, Error1} ->
            io:format("Upload failed: ~p~n", [Error1])
    end,
    
    % List objects in the bucket
    io:format("Listing objects in bucket ~s...~n", [BucketName]),
    case aliyun_oss:list_objects(Bucket) of
        {ok, #{objects := Objects} = Result4} ->
            io:format("List successful: ~p~n", [Result4]),
            io:format("Objects: ~p~n", [Objects]);
        {error, Error4} ->
            io:format("List failed: ~p~n", [Error4])
    end,
    
    ok. 