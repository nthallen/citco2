function pccptbstale(varargin)
% pccptbstale( [...] );
% PTB stale
h = timeplot({'PTB_dev_stale','PTB_drv_stale'}, ...
      'PTB stale', ...
      'stale', ...
      {'PTB\_dev\_stale','PTB\_drv\_stale'}, ...
      varargin{:} );
