function cust_pccifshkt(h)
% cust_pccifshkt(h)
% Customize plot created by pccifshkt

% pccifshkt's definition:

% function pccifshkt(varargin)
% % pccifshkt( [...] );
% % IFS HK Temps
% h = timeplot({'ScBlkT'}, ...
%       'IFS HK Temps', ...
%       'Temps', ...
%       {'ScBlkT'}, ...
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
