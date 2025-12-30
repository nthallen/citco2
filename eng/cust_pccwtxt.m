function cust_pccwtxt(h)
% cust_pccwtxt(h)
% Customize plot created by pccwtxt

% pccwtxt's definition:

% function pccwtxt(varargin)
% % pccwtxt( [...] );
% % WTX Temp
% h = timeplot({'WTX_AirT','WTX_DewPt'}, ...
%       'WTX Temp', ...
%       'Temp', ...
%       {'WTX\_AirT','WTX\_DewPt'}, ...
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
