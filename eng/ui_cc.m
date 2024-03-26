function ui_cc(dirfunc, stream)
% ui_cc
% ui_cc(dirfunc [, stream])
% dirfunc is a string specifying the name of a function
%   that specifies where data run directories are stored.
% stream is an optional argument specifying which stream
%   the run directories have recorded, e.g. 'SerIn'
if nargin < 1
  dirfunc = 'CITCO2_Data_Dir';
end
if nargin >= 2
  f = ne_dialg(stream, 1);
else
  f = ne_dialg('citco2 Instrument',1);
end
f = ne_dialg(f, 'add', 0, 1, 'gcctm', 'T Mbase' );
f = ne_dialg(f, 'add', 1, 0, 'pcctmtd', 'T Drift' );
f = ne_dialg(f, 'add', 1, 0, 'pcctmcpu', 'CPU' );
f = ne_dialg(f, 'add', 1, 0, 'pcctmram', 'RAM' );
f = ne_dialg(f, 'add', 1, 0, 'pcctmd', 'Disk' );
f = ne_dialg(f, 'add', 0, 1, 'gccptb', 'PTB' );
f = ne_dialg(f, 'add', 1, 0, 'pccptbp', 'P' );
f = ne_dialg(f, 'add', 1, 0, 'pccptbt', 'T' );
f = ne_dialg(f, 'add', 1, 0, 'pccptbstale', 'stale' );
f = ne_listdirs(f, dirfunc, 15);
f = ne_dialg(f, 'newcol');
ne_dialg(f, 'resize');
