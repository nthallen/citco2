function pccstratio(varargin);
% pccstratio( [...] );
% ST Ratio
h = timeplot({'radiance_ratio','Rad_Open_Pct','Rad_Close_Pct'}, ...
      'ST Ratio', ...
      'Ratio', ...
      {'radiance\_ratio','Rad\_Open\_Pct','Rad\_Close\_Pct'}, ...
      varargin{:} );
