function pccwtx_precipdsc(varargin)
% pccwtx_precipdsc( [...] );
% WTX Precip DS 2C
h = ne_dstat({
  'STEnc_DS_2C', 'STEnc_status', 6 }, 'DS 2C', varargin{:} );
