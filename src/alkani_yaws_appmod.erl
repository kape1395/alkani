%/--------------------------------------------------------------------
%| Copyright 2016 Karolis Petrauskas
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%%
%%% YAWS Appmod.
%%% See `http://www.infoq.com/articles/vinoski-erlang-rest`.
%%%
-module(alkani_yaws_appmod).
-compile([{parse_transform, lager_transform}]).
-export([out/1]).
-include_lib("yaws/include/yaws_api.hrl").

-define(URL_ROOT, "alkani").
-define(GUI_PATH, "/alkani/").


%%% ============================================================================
%%% API Functions
%%% ============================================================================

%%
%%  Entry point for Yaws appmod.
%%
out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),
    Method = yaws_api:http_request_method(Arg#arg.req),
    {Ip, _Port} = Arg#arg.client_ip_port,
    case lists:member({"alkani_debug", "true"}, Arg#arg.opaque) of
        true -> lager:debug("Handling request: path=~p, method=~p, from ip: ~p", [Path, Method, encode_ip(Ip)]);
        false -> ok
    end,
    handle_request(Path, Method, Arg).



%%% ============================================================================
%%% Handlers for REST URIs
%%% ============================================================================

%
%   Player socket.
%
handle_request([?URL_ROOT, "player"], 'GET', Arg) ->
    alkani_player:start(Arg);

%
%   User interface / static files.
%
handle_request([], _Method, _Arg) ->
    {redirect_local, ?GUI_PATH};

handle_request([?URL_ROOT], 'GET', Arg) ->
    handle_request([?URL_ROOT, "index.html"], 'GET', Arg);

handle_request(["favicon.ico"], 'GET', Arg) ->
    handle_request([?URL_ROOT, "favicon.ico"], 'GET', Arg);

handle_request([?URL_ROOT, "js", "conf.js"] = Path, 'GET', Arg) ->
    FileName = string:join(Path, "/"),
    Response = serve_priv_file(FileName, yaws_api:mime_type(FileName), Arg),
    case lists:member({status, 200}, Response) of
        true ->
            Preprocess = fun
                ({content, ContentType, Content}) ->
                    AppVersion = erlang:iolist_to_binary(alkani_app:version()),
                    BinContent = erlang:iolist_to_binary(Content),
                    NewContent = binary:replace(BinContent, <<"@VERSION@">>, AppVersion, [global]),
                    {content, ContentType, NewContent};
                (Other) ->
                    Other
            end,
            lists:map(Preprocess, Response);
        false ->
            Response
    end;

handle_request([?URL_ROOT | Path], 'GET', Arg) ->
    FileName = string:join(Path, "/"),
    serve_priv_file(FileName, yaws_api:mime_type(FileName), Arg);

%
%   Other.
%
handle_request(Path, Method, Arg) ->
    lager:warning("Unknown request: path=~p, method=~p, args=~p", [Path, Method, Arg]),
    [
        {status, 400},
        {content, "text/html", "Not found."}
    ].



%%% ============================================================================
%%% Internal functions
%%% ============================================================================

%%
%%  Prints IP.
%%
encode_ip({One, Two, Three, Four}) ->
    lists:flatten(io_lib:format("~B.~B.~B.~B", [One, Two, Three, Four])).


%%
%%  Generic method for responding to unknown paths.
%%
respond_unknown(Module, Path, Method, Args) ->
    respond_error(404, "Unknown resource", Module, Path, Method, Args).


%%
%%  Respond to request with error message.
%%
respond_error(Number, Message, Module, Path, Method, Args) ->
    lager:warning(string:concat(Message, " in ~p, path=~p, method=~p, args=~p"), [Module, Path, Method, Args]),
    [
        {status, Number},
        {ehtml, get_error_html(Message)}
    ].


%%
%%
%%
get_error_html(Error) ->
    [{h1,[],"ERROR!"},
    {p,[],Error}].


%%
%%
%%
serve_priv_file(FileName, ContentType, Arg) ->
    PrivDir = case code:priv_dir(alkani_app:name()) of
        {error, bad_name} -> "priv"; % To allow testing without creating whole app.
        Dir -> Dir
    end,
    AbsolutePath = lists:flatten(PrivDir ++ "/www/" ++ FileName),
    case file:read_file(AbsolutePath) of
        {ok, Content} ->
            ContentType = yaws_api:mime_type(FileName),
            [
                {status, 200},
                {content, ContentType, Content}
            ];
        {error, enoent} ->
            respond_unknown(?MODULE, FileName, 'GET', Arg);
        {error, eacces} ->
            respond_error(401, "Insuficient acces rights", ?MODULE, FileName, 'GET', Arg);
        {error, Reason} ->
            respond_error(400, lists:concat(["Error: ", Reason]), ?MODULE, FileName, 'GET', Arg)
    end.


