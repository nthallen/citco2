function pcces(varargin);
% pcces( [...] );
% Enclosure Status
h = ne_dstat({
  'STEnc_Open_Limit', 'STEnc_status', 0; ...
	'STEnc_Close_Limit', 'STEnc_status', 1; ...
	'STEnc_Open_Relay', 'STEnc_status', 3; ...
	'STEnc_Close_Relay', 'STEnc_status', 4 }, 'Status', varargin{:} );
