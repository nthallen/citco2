function pccwtx_precipa(varargin)
% pccwtx_precipa( [...] );
% WTX Precip Accum
h = timeplot({'WTX_PrecipAcc'}, ...
      'WTX Precip Accum', ...
      'Accum', ...
      {'WTX\_PrecipAcc'}, ...
      varargin{:} );
