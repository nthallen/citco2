function cust_pccmkst(h)
% cust_pccmkst(h)
% Customize plot created by pccmkst

% pccmkst's definition:

% function pccmkst(varargin)
% % pccmkst( [...] );
% % MKS925 T
% h = timeplot({'MKS_T'}, ...
%       'MKS925 T', ...
%       'T', ...
%       {'MKS\_T'}, ...
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
