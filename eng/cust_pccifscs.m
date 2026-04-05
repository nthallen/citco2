function cust_pccifscs(h)
% cust_pccifscs(h)
% Customize plot created by pccifscs

% pccifscs's definition:

% function pccifscs(varargin)
% % pccifscs( [...] );
% % IFS C Stat
% h = timeplot({'IFSCStat'}, ...
%       'IFS C Stat', ...
%       'C Stat', ...
%       {'IFSCStat'}, ...
%       varargin{:} );

% Example customizations include:
set(h,'LineStyle','none','Marker','.');
%   ax = get(h(1),'parent');
%   set(ax,'ylim',[0 800]);

% What I want to do with this is:
% Identify the unique values in the ydata and get the associated strings
% from IFSCStat.tmc.
% Map the ydata onto a contiguous integers and replace the ydata with the
% contiguously mapped values
% Set ylabels for those integers using the strings
try
    fname = [ getrundir filesep 'IFSCStat.mat' ];
    load(fname, 'IFSCStat');
    YData = h.YData;
    [C, ~, ic] = unique(YData);
    h.YData = ic;
    N = length(C);
    % C are the N unique values
    % ic are the indexes within C for each of the YData
    %  hence the effective map from YData onto 1:N
    %IFSCStat = {}; will be an array of IFSCStat strings
    % This should be parsed at runtime from the file saved with the run.
    % To make that work, getrun would need an option to pull that file down
    % Or it could be parsed during the MATLAB portion of getrun
    % load('260319.1/IFSCStat.mat');
    % The YData values are indexes into a zero-based array,
    % but IFSCStat is one-based, so we need to add 1:
    labels = IFSCStat(C+1);
    ax = h.Parent;
    set(ax,'YTick', 1:N, 'YTickLabel', labels, 'ylim', [0.75 N+0.25]);
catch
end