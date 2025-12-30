function cust_pcctmd(h)
% cust_pcctmd(h)
% Customize plot created by pcctmd

% pcctmd's definition:

% function pcctmd(varargin)
% % pcctmd( [...] );
% % T Mbase Disk
% h = timeplot({'Disk'}, ...
%       'T Mbase Disk', ...
%       'Disk', ...
%       {'Disk'}, ...
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
