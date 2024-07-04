function pccste(varargin);
% pccste( [...] );
% ST Elevation
h = timeplot({'ST_tpg_ele','Sol_ele'}, ...
      'ST Elevation', ...
      'Elevation', ...
      {[0 1 -1.1 1.1], 'ST','Model'}, ...
      varargin{:} );
