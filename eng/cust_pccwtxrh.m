function cust_pccwtxrh(h)
% cust_pccwtxrh(h)
% Customize plot created by pccwtxrh

% pccwtxrh's definition:

% function pccwtxrh(varargin)
% % pccwtxrh( [...] );
% % WTX RH
% h = timeplot({'RH_Open_Pct','RH_Close_Pct','WTX_RH'}, ...
%       'WTX RH', ...
%       'RH', ...
%       {'RH\_Open\_Pct','RH\_Close\_Pct','WTX\_RH'}, ...
%       varargin{:} );

% Example customizations include:
%   set(h,'LineStyle','none','Marker','.');
%   ax = get(h(1),'parent');
%   set(ax,'ylim',[0 800]);
ax = get(h(1),'parent');
lns = ax.Children;
for i=1:length(lns)
    ln = lns(i);
    N = find(ln.YData,1)-1;
    ln.YData(1:N) = NaN;
end
