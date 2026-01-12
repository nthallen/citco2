function cust_pccmksp(h)
% cust_pccmksp(h)
% Customize plot created by pccmksp

% pccmksp's definition:

% function pccmksp(varargin)
% % pccmksp( [...] );
% % MKS925 P
% h = timeplot({'Pump_P'}, ...
%       'MKS925 P', ...
%       'P', ...
%       {'Pump\_P'}, ...
%       varargin{:} );

% Example customizations include:
%   set(h,'LineStyle','none','Marker','.');
%   ax = get(h(1),'parent');
%   set(ax,'ylim',[0 800]);
ax = get(h(1),'parent');
lns = ax.Children;
for i=1:length(lns)
    ln = lns(i);
    ln.YData(ln.YData == 0) = NaN;
end
