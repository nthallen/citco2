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
f = ne_dialg(f, 'add', 0, 1, 'gccwtx', 'WTX' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxt', 'Temp' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxrh', 'RH' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxp', 'P' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxsr', 'Sol Rad' );
f = ne_dialg(f, 'add', 0, 1, 'gccwtxw', 'WTX Wind' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxwd', 'Dir' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxws', 'Speed' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxwq', 'Quality' );
f = ne_dialg(f, 'newcol');
f = ne_dialg(f, 'add', 0, 1, 'gccwtx_precip', 'WTX Precip' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtx_precipt', 'Type' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtx_precipa', 'Accum' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtx_precipi', 'Intensity' );
f = ne_dialg(f, 'add', 0, 1, 'gccwtxs', 'WTX Status' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxsf', 'Fresh' );
f = ne_dialg(f, 'add', 1, 0, 'pccwtxss', 'Stale' );
f = ne_dialg(f, 'add', 0, 1, 'gccst', 'ST' );
f = ne_dialg(f, 'add', 1, 0, 'pccstt', 'Tdrift' );
f = ne_dialg(f, 'add', 1, 0, 'pccstf', 'Flip' );
f = ne_dialg(f, 'add', 1, 0, 'pccstm', 'Modus' );
f = ne_dialg(f, 'add', 1, 0, 'pccsts', 'Stale' );
f = ne_dialg(f, 'add', 1, 0, 'pccstt_int', 't int' );
f = ne_dialg(f, 'add', 1, 0, 'pccsta', 'Azimuth' );
f = ne_dialg(f, 'add', 1, 0, 'pccste', 'Elevation' );
f = ne_listdirs(f, dirfunc, 15);
f = ne_dialg(f, 'newcol');
ne_dialg(f, 'resize');
