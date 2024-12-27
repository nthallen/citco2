function pccststatus(varargin)
% pccststatus( [...] );
% ST Status
h = ne_dstat({
  'OK2Open', 'ok_to_open', 0; ...
	'ST_flip_bit', 'ST_flip', 0 }, 'Status', varargin{:} );
