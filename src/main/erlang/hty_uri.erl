%% Author: jens       
%% Created: 14 feb 2013
%% Description: TODO: Add description to hty_uri
-module(hty_uri).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([parse_path/1, pathzipper/1]).
-export([matrix/3, matrix/2, pack/1, append/2]).

%%
%% API Functions
%%
-spec parse_path(binary()) -> {[Segments::(binary()|{binary, []})], [QueryParams::binary()]}.
parse_path(Path) when is_binary(Path) ->
	parse_path([], Path, false).

matrix({Above, [Segment|Below]}, Key, Value) ->
    Segment1 = case Segment of
		   {Name, Matrix} ->
		       {Name, lists:keystore(Key, 1, Matrix, {Key, Value})};
		   Name ->
		       {Name, [{Key, Value}]}
	       end,
    %io:format("New segment after matrix() = ~p~n", [Segment1]),
    {Above, [Segment1|Below]}.

matrix({_Above, [Segment|_Below]}, Key) ->
    case Segment of
	{_Name, Matrix} ->
	    case lists:keyfind(Key, 1, Matrix) of
		false ->
		    no;
		{_Key, Value} ->
		    Value
	    end;
	_Name ->
	    no
    end.
		
append({Above, Below}, Leaf) ->
    Reversed = lists:reverse(Below),
    {Above, lists:reverse([Leaf|Reversed])}.

pack({Above, Below}) ->
    Segs = hty_util:rewind(Above, Below),
    lists:flatmap(fun({Name, Matrix}) -> 
		      [$/, Name|lists:map(fun({Key, Value}) -> 
					       [$;, Key, $=, Value]
				      end, Matrix)];
		 (Name) ->
		      [$/, Name]
	      end, Segs).

%%
%% Local Functions
%%
parse_path(Segs, Input, Matrix) ->
	{Token, Rest} = hty_scan:until_oneof(Input, $/, $?, $;),
	Segs1 = case Token of
			<<>> ->
				Segs;
			_ ->
                            case Matrix of
                                true ->
				    {MKey, MRest} = hty_scan:until(Token, $=),
				    M = case MRest of
					    <<$=, MVal/binary>> ->
						{MKey, MVal};
					    <<>> ->
						{MKey, <<>>}
					end,
                                    case Segs of
                                        [{Seg, Ms}|Ss] ->
                                            [{Seg, [M|Ms]}|Ss];
                                        [Seg|Ss] ->
                                            [{Seg, [M]}|Ss]
                                    end;
                                false ->
                                    [Token|Segs]
                            end
		end,
	case Rest of
		<<$/, Input1/binary>> ->
			parse_path(Segs1, Input1, false);
		<<$?, Input1/binary>> ->
			Segs2 = lists:reverse(Segs1),
			parse_query([], Input1, Segs2);
                <<$;, Input1/binary>> ->
                        parse_path(Segs1, Input1, true);
		<<>> ->
			Segs2 = lists:reverse(Segs1),
			{Segs2, []}
	end.

parse_query(Params, Input, Segs) ->
	{Token, Rest} = hty_scan:until(Input, $&),
	KeyValue = case hty_scan:until(Token, $=) of  
							 {Key, <<$=, Value/binary>>} ->
								 {Key, Value};
							 Kv ->
								 Kv
						 end,
	Params1 = [KeyValue|Params],
	case Rest of
		<<$&, Input1>> ->
			parse_query(Params1, Input1, Segs);
		<<>> ->
			{Segs, lists:reverse(Params1)}
	end.

pathzipper(Segments) ->
	{[], lists:map(fun(Seg) when is_binary(Seg) ->
						   binary_to_list(Seg); 
					  ({Name, Matrix}) ->
						   {binary_to_list(Name), Matrix}
				   end, Segments)}.
