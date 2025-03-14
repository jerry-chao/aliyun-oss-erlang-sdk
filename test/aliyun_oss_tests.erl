%%%-------------------------------------------------------------------
%%% @doc Tests for Aliyun OSS Erlang SDK
%%% @end
%%%-------------------------------------------------------------------
-module(aliyun_oss_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test cases
auth_test() ->
    % Test creating auth object
    Auth1 = aliyun_oss:auth(<<"test_id">>, <<"test_secret">>),
    ?assertMatch({auth, _, _, _}, Auth1),
    
    % Test creating auth object with security token
    Auth2 = aliyun_oss:auth(<<"test_id">>, <<"test_secret">>, <<"test_token">>),
    ?assertMatch({auth, _, _, _}, Auth2).

bucket_test() ->
    % Create auth object
    Auth = aliyun_oss:auth(<<"test_id">>, <<"test_secret">>),
    
    % Test creating bucket object
    Bucket = aliyun_oss:bucket(Auth, <<"http://oss-cn-hangzhou.aliyuncs.com">>, <<"test-bucket">>, <<"oss-cn-hangzhou">>),
    ?assertMatch({bucket, _, _, _, _, _}, Bucket).

% Mock tests with meck
mock_test_() ->
    {setup,
     fun() -> 
         % Setup mocks
         meck:new(hackney, [passthrough]),
         meck:expect(hackney, request, 
             fun(put, _, _, _, _) -> 
                 {ok, 200, [{<<"ETag">>, <<"\"test-etag\"">>}], make_ref()} 
             end),
         meck:expect(hackney, body, fun(_) -> {ok, <<>>} end)
     end,
     fun(_) -> 
         % Teardown mocks
         meck:unload(hackney)
     end,
     [
         {"Test put_object",
          fun() ->
              % Create auth and bucket objects
              Auth = aliyun_oss:auth(<<"test_id">>, <<"test_secret">>),
              Bucket = aliyun_oss:bucket(Auth, <<"http://oss-cn-hangzhou.aliyuncs.com">>, <<"test-bucket">>, <<"oss-cn-hangzhou">>),
              
              % Test put_object
              Result = aliyun_oss:put_object(Bucket, <<"test.txt">>, <<"Hello, OSS!">>),
              ?assertMatch({ok, _}, Result),
              
              % Verify hackney:request was called
              ?assert(meck:called(hackney, request, '_'))
          end}
     ]
    }. 