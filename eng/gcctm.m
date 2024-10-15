function fig = gcctm(varargin);
% gcctm(...)
% T Mbase
ffig = ne_group(varargin,'T Mbase','pcctmsws','pcctmmfc','pcctmtd','pcctmcpu','pcctmram','pcctmd');
if nargout > 0 fig = ffig; end
