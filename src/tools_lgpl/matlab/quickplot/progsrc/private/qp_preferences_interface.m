function fig=qp_preferences_interface
%QP_PREFERENCES_INTERFACE Show QuickPlot preferences user interface.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2012 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

ListWidth=100;
Margin=10;
HOffset=ListWidth+2*Margin;
TabWidth=290;
WinHeight=300;
TabHeight=WinHeight-3*Margin-20;

screensize=get(0,'screensize');
dims=[HOffset+TabWidth WinHeight];
PosLL=floor((screensize(3:4)-dims)/2);
%PosLL=qp_settings('PrefPos',PosLL);
pos(1:2)=PosLL;
pos(3:4)=dims;

Inactive=get(0,'defaultuicontrolbackgroundcolor');
Active=[1 1 1];

mfig = qp_uifigure('Preferences','','PreferenceFig',pos);
set(mfig,'userdata',1)
%=============
Panes={};
%=============
handles=[]; VOffset = dims(2)-30;
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset-2 160 18], ...
    'string','User Interface Font', ...
    'horizontalalignment','left', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','pushbutton', ...
    'position',[HOffset+170 VOffset 60 20], ...
    'string','Select ...', ...
    'callback','d3d_qp changefont', ...
    'parent',mfig);
depdir = qp_basedir('deploy');
if ~isempty(depdir)
    VOffset = VOffset-25;
    handles(end+1)=uicontrol('style','text', ...
        'position',[HOffset VOffset-2 100 18], ...
        'string','Work Directory', ...
        'horizontalalignment','left', ...
        'parent',mfig);
    VOffset = VOffset-20;
    handles(end+1)=uicontrol('style','edit', ...
        'position',[HOffset VOffset TabWidth-Margin 20], ...
        'string',depdir, ...
        'horizontalalignment','left', ...
        'enable','inactive', ...
        'parent',mfig);
end

VOffset = VOffset-25;
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset-2 100 18], ...
    'string','Organization Name', ...
    'horizontalalignment','left', ...
    'parent',mfig);
VOffset = VOffset-20;
handles(end+1)=uicontrol('style','edit', ...
    'position',[HOffset VOffset TabWidth-Margin 20], ...
    'string',qp_settings('organizationname'), ...
    'horizontalalignment','left', ...
    'backgroundcolor',Active, ...
    'tag','organizationname', ...
    'callback','d3d_qp organizationname', ...
    'enable','on', ...
    'parent',mfig);
%--------------------------------------------------------------------------
Panes(end+1,1:2)={'General' handles};
%=============
handles=[]; VOffset = dims(2)-Margin-20;
%--------------------------------------------------------------------------
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset-2 150 18], ...
    'string','Quick View Figure Layout', ...
    'horizontalalignment','left', ...
    'parent',mfig);
VOffset = VOffset-25;
df = length(handles);
handles(end+1)=uicontrol('style','radiobutton', ...
    'position',[HOffset+20 VOffset-2 150 20], ...
    'string','Default New Figure', ...
    'tag','defaultnewfigure', ...
    'callback','d3d_qp defaultnewfigure', ...
    'value',0, ...
    'horizontalalignment','left', ...
    'parent',mfig);
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset+37 VOffset-2 150 18], ...
    'string','Figure Colour', ...
    'backgroundcolor',get(mfig,'color'), ...
    'tag','defaultfigurecolortext', ...
    'horizontalalignment','left', ...
    'enable','off', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','pushbutton', ...
    'position',[HOffset+170 VOffset 25 20], ...
    'callback','d3d_qp defaultfigurecolor', ...
    'enable','off', ...
    'parent',mfig);
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','radiobutton', ...
    'position',[HOffset+20 VOffset-2 150 20], ...
    'string','Read from File', ...
    'tag','defaultloadfigure', ...
    'callback','d3d_qp defaultloadfigure', ...
    'value',0, ...
    'horizontalalignment','left', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','pushbutton', ...
    'position',[HOffset+170 VOffset 60 20], ...
    'string','Select ...', ...
    'callback','d3d_qp defaultfigure', ...
    'enable','off', ...
    'parent',mfig);
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','edit', ...
    'position',[HOffset+37 VOffset-2 dims(2)-58 20], ...
    'backgroundcolor',get(mfig,'color'), ...
    'horizontalalignment','left', ...
    'enable','off', ...
    'tag','defaultfigure', ...
    'parent',mfig);
defig = qp_settings('defaultfigure');
if isempty(defig)
    set(handles(df+1),'value',1)
    set(handles(df+(2:3)),'enable','on')
    set(handles(df+3),'backgroundcolor',qp_settings('defaultfigurecolor')/255)
else
    set(handles(df+4),'value',1)
    set(handles(df+5),'enable','on')
    set(handles(df+6),'string',defig,'enable','inactive')
end
%-------------
VOffset = VOffset-35;
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset 150 18], ...
    'string','Default Axes Layout', ...
    'backgroundcolor',get(mfig,'color'), ...
    'tag','defaultaxeslayouttext', ...
    'horizontalalignment','left', ...
    'enable','on', ...
    'parent',mfig);
%-------------
VOffset = VOffset-20;
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset+37 VOffset-2 150 18], ...
    'string','Colour', ...
    'backgroundcolor',get(mfig,'color'), ...
    'tag','defaultaxescolortext', ...
    'horizontalalignment','left', ...
    'enable','on', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','pushbutton', ...
    'position',[HOffset+170 VOffset 25 20], ...
    'callback','d3d_qp defaultaxescolor', ...
    'backgroundcolor',qp_settings('defaultaxescolor')/255, ...
    'enable','on', ...
    'parent',mfig);
%-------------
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','checkbox', ...
    'position',[HOffset+20 VOffset-2 150 18], ...
    'string','Closed Bounding Box', ...
    'value',qp_settings('boundingbox'), ...
    'callback','d3d_qp boundingbox', ...
    'parent',mfig);
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','edit', ...
    'position',[HOffset VOffset 35 20], ...
    'string',num2str(qp_settings('colorbar_ratio')), ...
    'backgroundcolor',Active, ...
    'horizontalalignment','right', ...
    'callback','d3d_qp colorbar_ratio', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset+37 VOffset-2 200 18], ...
    'string','Length/Width Ratio of Colour Bar', ...
    'horizontalalignment','left', ...
    'parent',mfig);
%-------------
if matlabversionnumber >= 7
    VOffset = VOffset-25;
    handles(end+1)=uicontrol('style','checkbox', ...
        'position',[HOffset+20 VOffset-2 200 18], ...
        'string','MATLAB v6 Zoom Behaviour', ...
        'value',qp_settings('v6zoombehavior'), ...
        'callback','d3d_qp v6zoombehavior', ...
        'parent',mfig);
end
%--------------------------------------------------------------------------
Panes(end+1,1:2)={'Quick View' handles};
set(handles,'visible','off')
%=============
handles=[]; VOffset = dims(2)-Margin-20;
%--------------------------------------------------------------------------
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset-2 150 18], ...
    'string','Background Colour', ...
    'horizontalalignment','left', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','pushbutton', ...
    'position',[HOffset+170 VOffset 25 20], ...
    'backgroundcolor',qp_settings('gridviewbackgroundcolor')/255, ...
    'callback','d3d_qp gridviewbackgroundcolor', ...
    'parent',mfig);
%-------------
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset-2 150 18], ...
    'string','Grid Colour', ...
    'horizontalalignment','left', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','pushbutton', ...
    'position',[HOffset+170 VOffset 25 20], ...
    'backgroundcolor',qp_settings('gridviewgridcolor')/255, ...
    'callback','d3d_qp gridviewgridcolor', ...
    'parent',mfig);
%-------------
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset-2 150 18], ...
    'string','Selection Colour', ...
    'horizontalalignment','left', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','pushbutton', ...
    'position',[HOffset+170 VOffset 25 20], ...
    'backgroundcolor',qp_settings('gridviewselectioncolor')/255, ...
    'callback','d3d_qp gridviewselectioncolor', ...
    'parent',mfig);
%-------------
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset-2 150 18], ...
    'string','Land Boundary Colour', ...
    'horizontalalignment','left', ...
    'parent',mfig);
handles(end+1)=uicontrol('style','pushbutton', ...
    'position',[HOffset+170 VOffset 25 20], ...
    'backgroundcolor',qp_settings('gridviewlandboundarycolor')/255, ...
    'callback','d3d_qp gridviewlandboundarycolor', ...
    'parent',mfig);
%-------------
VOffset = VOffset-25;
handles(end+1)=uicontrol('style','checkbox', ...
    'position',[HOffset VOffset-2 150 18], ...
    'string','Show Grid Indices', ...
    'value',qp_settings('gridviewshowindices'), ...
    'callback','d3d_qp gridviewshowindices', ...
    'parent',mfig);
%--------------------------------------------------------------------------
Panes(end+1,1:2)={'Grid View' handles};
set(handles,'visible','off')
%=============
handles=[]; VOffset = dims(2)-Margin-20;
%--------------------------------------------------------------------------
handles(end+1)=uicontrol('style','text', ...
    'position',[HOffset VOffset-2 150 18], ...
    'string','Select File Filters', ...
    'horizontalalignment','left', ...
    'parent',mfig);
FF = qp_filefilters('all');
FS = qp_filefilters('selected');
[FS,iFull] = intersect(FF(:,2),FS(:,2));
handles(end+1)=uicontrol('style','listbox', ...
    'position',[HOffset VOffset-TabHeight+20 TabWidth-Margin TabHeight-20], ...
    'string',FF(:,2), ...
    'callback','d3d_qp filefilterselection', ...
    'tag','filefilterselection', ...
    'max',2, ...
    'value',iFull, ...
    'userdata',iFull, ...
    'backgroundcolor',Active, ...
    'parent',mfig);
%--------------------------------------------------------------------------
Panes(end+1,1:2)={'File Filters' handles};
set(handles,'visible','off')
%=============
if qp_settings('debugging')
    handles=[]; VOffset = dims(2)-Margin-20;
%--------------------------------------------------------------------------
    handles(end+1)=uicontrol('style','checkbox', ...
        'position',[HOffset VOffset-2 150 18], ...
        'string','Show Inactive Options', ...
        'value',qp_settings('showinactiveopt'), ...
        'callback','d3d_qp showinactiveopt', ...
        'parent',mfig);
%--------------------------------------------------------------------------
    Panes(end+1,1:2)={'Debugging' handles};
    set(handles,'visible','off')
end
%=============
%handles=[]; VOffset = dims(2)-Margin-20;
%--------------------------------------------------------------------------
%Panes(end+1,1:2)={'New Pane' handles};
%set(handles,'visible','off')
%=============
uicontrol('style','listbox', ...
    'tag','PaneList', ...
    'position',[Margin Margin ListWidth dims(2)-2*Margin], ...
    'string',Panes(:,1), ...
    'userdata',Panes(:,2), ...
    'backgroundcolor',Active, ...
    'callback','d3d_qp prefpane', ...
    'parent',mfig);
uicontrol('style','pushbutton', ...
    'position',[dims(1)-50-Margin Margin 50 20], ...
    'string','OK', ...
    'callback','closereq', ...
    'parent',mfig);
%=============
set(mfig,'visible','on','windowstyle','modal')
%=============
if nargout>0
    fig=mfig;
end
