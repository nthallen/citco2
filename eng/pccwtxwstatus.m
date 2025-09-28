function pccwtxwstatus(varargin)
% pccwtxwstatus( [...] );
% WTX Wind Status
h = ne_dstat({
  'is_high', 'ws_high', 0; ...
	'is_low', 'ws_low', 0 }, 'Status', varargin{:} );
