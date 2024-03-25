function ui_eng(dirfunc, stream)
% ui_eng
% ui_eng(dirfunc [, stream])
% dirfunc is a string specifying the name of a function
%   that specifies where data run directories are stored.
% stream is an optional argument specifying which stream
%   the run directories have recorded, e.g. 'SerIn'
if nargin < 1
  dirfunc = 'C:/home/Exp';
end
if nargin >= 2
  f = ne_dialg(stream, 1);
else
  f = ne_dialg('Engineering Data',1);
end
f = ne_dialg(f, 'add', 0, 1, 'gengtm', 'T Mbase' );
f = ne_dialg(f, 'add', 1, 0, 'pengtmtd', 'T Drift' );
f = ne_dialg(f, 'add', 1, 0, 'pengtmcpu', 'CPU' );
f = ne_dialg(f, 'add', 1, 0, 'pengtmram', 'RAM' );
f = ne_dialg(f, 'add', 1, 0, 'pengtmd', 'Disk' );
f = ne_dialg(f, 'add', 0, 1, 'gengptb', 'PTB' );
f = ne_dialg(f, 'add', 1, 0, 'pengptbp', 'P' );
f = ne_dialg(f, 'add', 1, 0, 'pengptbt', 'T' );
f = ne_dialg(f, 'add', 1, 0, 'pengptbstale', 'stale' );
f = ne_listdirs(f, dirfunc, 15);
f = ne_dialg(f, 'newcol');
ne_dialg(f, 'resize');
