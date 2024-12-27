function pcces(varargin)
% pcces( [...] );
% Enclosure Status
h = ne_dstat({
  'STEnc_Close_Limit', 'STEnc_status', 0; ...
	'STEnc_Open_Limit', 'STEnc_status', 1; ...
	'STEnc_Operating', 'STEnc_status', 2; ...
	'STEnc_Weather', 'STEnc_status', 3; ...
	'STEnc_Power', 'STEnc_status', 4; ...
	'STEnc_Error', 'STEnc_status', 7; ...
	'STEnc_Open_Relay', 'STEnc_status', 8; ...
	'STEnc_Close_Relay', 'STEnc_status', 9 }, 'Status', varargin{:} );
