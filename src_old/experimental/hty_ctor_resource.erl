%% Author: jens
%% Created: 16 mar 2013
%% Description: TODO: Add description to hty_ctor_resource
-module(hty_ctor_resource, [NameTemplate, Subs]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%
handle(Htx) ->
	case Htx:method() of 
		'POST' ->
			%Hmm. Schema?
			Schema = meh_hmmm,
			Htx:recvform(Schema, fun onform/2);
		_ ->
			Htx:method_not_allowed(['POST'])
	end.

		
%%
%% Local Functions
%%

onform(_Form, _) ->
		  % Vi har inte htx har!
			Htx = ojda,
			Storage = Htx:storage(),
			Htx1 = Htx:method('GET'),
			_Htx2 = Htx1:dispatch(Subs),
			case Htx1:status() of
				{200, _} ->
					%Vad är storage? Hur koppa datat?
					Name = NameTemplate:ngt(),
					Storage:save(Name, Htx1:outs())		
			end.

