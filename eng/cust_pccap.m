function cust_pccap(h)
% cust_pccap(h)
% Customize plot created by pccap
ne_display_state(h, 'AlgoP1');
return

% pccap's definition:

% function pccap(varargin)
% % pccap( [...] );
% % Algo P1
% h = timeplot({'AlgoP1'}, ...
%       'Algo P1', ...
%       'P1', ...
%       {'AlgoP1'}, ...
%       varargin{:} );

% Example customizations include:
%   set(h,'LineStyle','none','Marker','.');
%   ax = get(h(1),'parent');
%   set(ax,'ylim',[0 800]);
