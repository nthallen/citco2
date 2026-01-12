function cust_pccwtxp(h)
% cust_pccwtxp(h)
% Customize plot created by pccwtxp

% pccwtxp's definition:

% function pccwtxp(varargin)
% % pccwtxp( [...] );
% % WTX P
% h = timeplot({'WTX_AbsAirP'}, ...
%       'WTX P', ...
%       'P', ...
%       {'WTX\_AbsAirP'}, ...
%       varargin{:} );

% Example customizations include:
%   set(h,'LineStyle','none','Marker','.');
%   ax = get(h(1),'parent');
%   set(ax,'ylim',[0 800]);
ax = get(h(1),'parent');
ln = ax.Children;
ln.YData(ln.YData == 0) = NaN;

