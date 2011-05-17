function outdata=d3d_qp(cmd,varargin)
%D3D_QP QuickPlot user interface: plotting interface for Delft3D output data.
%   To start the interface type: d3d_qp
%
%   See also QPFOPEN, QPREAD.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C)  Stichting Deltares, 2011.                                     
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

%VERSION = 2.16
qpversionbase = 'v<VERSION>';
%
persistent qpversion logfile logtype

if isempty(qpversion)
    if isequal(qpversionbase(1:2),'v<')
        qpversion=['development code ' datestr(clock)];
    else
        qpversion=qpversionbase;
    end
    logfile=0;
    logtype=1;
end

T_=1; ST_=2; M_=3; N_=4; K_=5;
DimStr={'subfield ','timestep ','station ','M=','N=','K='};

if nargin==0
    cmd='initialize';
elseif ~ischar(cmd)
    ui_message('error','Invalid command: must be char array')
    return
end
cmd=lower(cmd);
if (nargout~=0)
    if strcmp(cmd,'initialize')
        outdata=[];
    elseif strcmp(cmd,'iswl')
        outdata=isequal(qp_settings('WLextensions','off'),'on');
        return
    elseif strcmp(cmd,'version')
        outdata=qpversion;
        return
    elseif isstandalone % allow standalone auto start ...
        outdata=[];
    elseif isempty(strmatch(cmd,{'loaddata','selectedfigure','selectedaxes','selecteditem'}))
        error('Too many output arguments.');
    end
end

if isempty(gcbf) || ~strcmp(get(gcbf,'tag'),'Delft3D-QUICKPLOT')
    mfig=findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
    
    if isempty(mfig) && isempty(strmatch(cmd,{'initialize','initialize_background','closefigure','printfigure','dayok'},'exact'))
        d3d_qp
        mfig=findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
        if isempty(mfig)
            %
            % if quickplot did not initialize, then stop
            %
            return
        end
    end
else
    mfig=gcbf;
end
UOH=[];
if ~isempty(mfig)
    UD=getappdata(mfig,'QPHandles');
    if ~isempty(UD)
        UOH=UD.Options.Handles;
        Inactive=UD.Inactive;
        Active=UD.Active;
    else
    end
else
    UD=[];
end

cmdargs=varargin;

try
    switch cmd
        case {'slider','startanim','animselect','animpush','stopanim'}
            qck_anim(cmd,cmdargs{:});
        case {'gridviewpoint','gridviewline','gridviewlineseg', ...
                'gridviewpiecewise','gridviewarbline','gridviewrange', ...
                'gridviewall','gridviewarbrect','gridviewarbarea', ...
                'gridviewpath'}
            qp_gridview(cmd,cmdargs{:});
        case 'plotmanagerresize'
            if ~isempty(UD)
                qp_plotmanager('resize',UD.PlotMngr);
            end
        case 'optionsresize'
            pos = get(gcbf,'pos');
            pos(3) = 180;
            if pos(4)<100
                pos(2) = pos(2) + pos(4) - 100;
                pos(4) = 100;
            end
            set(gcbf,'pos',pos)
            %
            update_option_positions(UD,pos(4)-30+1)
            %
            spos = get(UD.Options.Slider,'position');
            spos(4) = pos(4)-15;
            set(UD.Options.Slider,'position',spos)
            %
            dpos = get(UD.Options.Dock,'position');
            dpos(2) = pos(4)-15;
            set(UD.Options.Dock,'position',dpos)
        case {'initialize','initialize_background'}
            showUI = isequal(cmd,'initialize');
            mfig=findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
            %
            % To hide version number from QuickPlot title, include the
            % following line
            %
            % qpversion='';
            %
            if length(mfig)>1
                delete(mfig(2:length(mfig)));
                mfig=mfig(1);
            elseif isempty(mfig)
                mfig=qp_interface(showUI);
                if showUI && strcmp(qp_settings('PlotMngrVisible','off'),'on')
                    d3d_qp plotmngr
                end
                if strcmp(qp_settings('showversion','off'),'on')
                    set(mfig,'name',cat(2,'Delft3D-QUICKPLOT ',qpversion));
                end
                if qp_settings('v6zoombehavior') && matlabversionnumber >= 7
                    zoom v6 on
                end
                if isstandalone
                    set(findobj(mfig,'tag','loaddata'),'visible','off')
                    set(findobj(mfig,'tag','scriptfiletomcw'),'visible','off')
                else
                    set(findobj(mfig,'tag','aboutmatlab'),'visible','off')
                end
                qp_updaterecentfiles(mfig)
            end
            if showUI
                figure(mfig);
            end
            if isstandalone
               javaaddpath([qp_basedir('exe') filesep 'netcdfAll-4.1.jar'])
               try
                  CloseSplashScreen;
               end
            end
            d3d_qp updatedomains
            d3d_qp refreshfigs
            set(mfig,'CloseRequestFcn','d3d_qp close');
            
        case {'openfile','reloadfile','openurl'}
            OpenFile=findobj(mfig,'tag','openfile','type','uipushtool');
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            sel=get(Handle_SelectFile,'value');
            Str=get(Handle_SelectFile,'string');
            %
            NewRecord = [];
            if strcmp(cmd,'reloadfile')
                NewRecord = qp_refresh(File(sel));
            elseif isempty(cmdargs)
                if strcmp(cmd,'openurl')
                    [FI,FileName,Tp,Otherargs]=qp_fmem('openurl');
                else
                    pn=get(OpenFile,'userdata');
                    [FI,FileName,Tp,Otherargs]=qp_fmem('opennew',pn);
                end
            else
                [FI,FileName,Tp,Otherargs]=qp_fmem('open',cmdargs{:});
            end
            %
            if isempty(NewRecord)
                if isempty(FI)
                    return
                end
                NewRecord.QPF=1;
                NewRecord.Name=FileName;
                NewRecord.Data=FI;
                NewRecord.FileType=Tp;
                if isfield(FI,'Options')
                    NewRecord.Options=FI.Options;
                else
                    NewRecord.Options=0;
                end
                NewRecord.Otherargs=Otherargs;
                %
                if strcmp(cmd,'reloadfile') && File(sel).Options
                    [Chk,NewRecord]=qp_getdata(NewRecord,'optionstransfer',File(sel));
                end
            else
                FileName = NewRecord.Name;
                Otherargs = NewRecord.Otherargs;
            end
            %
            if isempty(File)
                Str={abbrevfn(FileName,60)};
                sel=1;
                File=NewRecord;
            else
                FileNameList={File.Name};
                sel=strmatch(FileName,FileNameList,'exact');
                if isempty(sel)
                    sel=length(File)+1;
                end
                Str{sel}=abbrevfn(FileName,60);
                File(sel)=NewRecord;
            end
            set(Handle_SelectFile,'userdata',File,'string',Str,'value',sel,'enable','on','backgroundcolor',Active);
            
            Handle_ReloadFile=findobj(mfig,'tag','reloadfile');
            set(Handle_ReloadFile,'enable','on');
            Handle_CloseFile=findobj(mfig,'tag','closefile');
            set(Handle_CloseFile,'enable','on');
            Handle_CloseAllFiles=findobj(mfig,'tag','closeallfiles');
            set(Handle_CloseAllFiles,'enable','on');
            set(findobj(mfig,'tag','fileinfo'),'enable','on');
            %
            RecentFiles = cell(9,1);
            for i = 1:9
                RecentFiles{i} = qp_settings(sprintf('File%2.2i',i),'');
            end
            ThisFile = sprintf('''%s'' ',FileName,Otherargs{:});
            ThisFile(end)=[];
            ii = strmatch(ThisFile,RecentFiles,'exact');
            if isempty(ii)
                ii = 9;
            else
                ii = ii(1);
            end
            for i = ii:-1:2
                if ~isempty(RecentFiles{i-1,1})
                    qp_settings(sprintf('File%2.2i',i),RecentFiles{i-1,1})
                    RecentFiles{i}=RecentFiles{i-1,1};
                end
            end
            qp_settings('File01',ThisFile)
            qp_updaterecentfiles(mfig)
            %
            if logfile
                writelog(logfile,logtype,cmd,FileName,Otherargs{:});
            end
            d3d_qp selectfile*
            
        case 'difffiles'
            pos = get(mfig,'position');
            pos(4) = 190;
            dfig = qp_uifigure('Diff Files','','Diff Files',pos);
            %
            voffset=pos(4)-29;
            uicontrol('Parent',dfig, ...
                'Enable','on', ...
                'Position',[11 voffset pos(3)-20 18], ...
                'String','Compare File', ...
                'Style','text', ...
                'Horizontalalignment','left', ...
                'Tag','txt_difffile2');
            voffset=voffset-20;
            L1 = uicontrol('Parent',dfig, ...
                'Callback','d3d_qp difffile1', ...
                'Enable','off', ...
                'Position',[11 voffset pos(3)-40 20], ...
                'String',' ', ...
                'BackgroundColor',Inactive, ...
                'Style','popupmenu', ...
                'Tag','difffile1', ...
                'Value',1);
            uicontrol('Parent',dfig, ...
                'Style','pushbutton', ...
                'CData',qp_icon('openfile','nan'), ...
                'Enable','on', ...
                'Position',[pos(3)-30 voffset 20 20], ...
                'String',' ', ...
                'Callback','d3d_qp openfile1', ...
                'Tooltip','Open a data file', ...
                'Tag','openfile1');
            %
            voffset=voffset-30;
            uicontrol('Parent',dfig, ...
                'Enable','on', ...
                'Position',[11 voffset pos(3)-20 18], ...
                'String','Reference File', ...
                'Style','text', ...
                'Horizontalalignment','left', ...
                'Tag','txt_difffile2');
            voffset=voffset-20;
            L2 = uicontrol('Parent',dfig, ...
                'Callback','d3d_qp difffile2', ...
                'Enable','off', ...
                'Position',[11 voffset pos(3)-40 20], ...
                'String',' ', ...
                'BackgroundColor',Inactive, ...
                'Style','popupmenu', ...
                'Tag','difffile2', ...
                'Value',1);
            uicontrol('Parent',dfig, ...
                'Style','pushbutton', ...
                'CData',qp_icon('openfile','nan'), ...
                'Enable','on', ...
                'Position',[pos(3)-30 voffset 20 20], ...
                'String',' ', ...
                'Callback','d3d_qp openfile2', ...
                'Tooltip','Open a data file', ...
                'Tag','openfile2');
            %
            voffset=voffset-30;
            uicontrol('Parent',dfig, ...
                'Enable','on', ...
                'Position',[11 voffset pos(3)-20 18], ...
                'String','Label', ...
                'Style','text', ...
                'Horizontalalignment','left', ...
                'Tag','txt_difflabel');
            voffset=voffset-20;
            Lb = uicontrol('Parent',dfig, ...
                'Callback','d3d_qp difflabel', ...
                'Enable','off', ...
                'Position',[11 voffset pos(3)-40 20], ...
                'String','Data File Equals Reference File', ...
                'BackgroundColor',Inactive, ...
                'HorizontalAlignment','left', ...
                'Style','edit', ...
                'Tag','difflabel', ...
                'Tooltip','Specify a name to identify this pair of files');
            setappdata(Lb,'LabelMode','auto')
            setappdata(Lb,'Label','')
            %
            voffset=voffset-30;
            width = (pos(3)-30)/2;
            uicontrol('Parent',dfig, ...
                'Style','pushbutton', ...
                'Enable','on', ...
                'Position',[11 voffset width 20], ...
                'String','Cancel', ...
                'Callback','d3d_qp diffcancel', ...
                'Tag','diffcancel');
            uicontrol('Parent',dfig, ...
                'Style','pushbutton', ...
                'Enable','off', ...
                'Position',[pos(3)-10-width voffset width 20], ...
                'String','Define', ...
                'Callback','d3d_qp diffdefine', ...
                'Tag','diffdefine', ...
                'Tooltip','Define new data resources as data in file A minus data in file B');
            %
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            Str=get(Handle_SelectFile,'string');
            NrInList=get(Handle_SelectFile,'value');
            if ~isempty(File)
                set([L1 L2], ...
                    'enable','on', ...
                    'BackgroundColor',Active, ...
                    'string',Str, ...
                    'value',NrInList)
                set(L1, ...
                    'userdata',File, ...
                    'tooltip', ['Select data file A' char(10) ...
                    ' ' char(10) ...
                    'Currently selected:' char(10) ...
                    File(NrInList).Name char(10) ...
                    'Type: ' File(NrInList).FileType])
                set(L2, ...
                    'tooltip', ['Select reference data file B' char(10) ...
                    ' ' char(10) ...
                    'Currently selected:' char(10) ...
                    File(NrInList).Name char(10) ...
                    'Type: ' File(NrInList).FileType])
            end
            %
            interactive = 1;
            if ~isempty(cmdargs)
                d3d_qp('difffile1',cmdargs{1})
                if nargin>2
                    d3d_qp('difffile2',cmdargs{2})
                    interactive = isequal(cmdargs{1},cmdargs{2});
                end
                if nargin>3
                    d3d_qp('difflabel',cmdargs{3})
                end
            end
            %
            if interactive
                set(dfig,'visible','on','windowstyle','modal')
                waitfor(dfig,'visible','off')
            else
                d3d_qp('diffdefine')
            end
            %
            Label = get(Lb,'string');
            Indices = get(dfig,'userdata');
            DiffFile = get(L1,'userdata');
            delete(dfig)
            %
            if ~isempty(Indices)
                FileName=Label;
                NewRecord.QPF=1;
                NewRecord.Name=FileName;
                NewRecord.Data=DiffFile(Indices);
                NewRecord.FileType='diff';
                NewRecord.Options=0;
                NewRecord.Otherargs={};
                %
                if isempty(File)
                    Str={abbrevfn(FileName,60)};
                    NrInList=1;
                    File=NewRecord;
                else
                    FileNameList={File.Name};
                    NrInList=strmatch(FileName,FileNameList,'exact');
                    if isempty(NrInList)
                        NrInList=length(File)+1;
                    end
                    Str{NrInList}=abbrevfn(FileName,60);
                    File(NrInList)=NewRecord;
                end
                set(Handle_SelectFile,'userdata',File,'string',Str,'value',NrInList,'enable','on','backgroundcolor',Active);
                d3d_qp selectfile*
                %
                if logfile
                    writelog(logfile,logtype,cmd,NewRecord.Data(1).Name,NewRecord.Data(2).Name,Label);
                end
            end
            
        case {'openfile1','openfile2'}
            hDiff = findall(0,'tag','Diff Files');
            if length(hDiff)~=1, return, end
            Handle_File1 = findobj(hDiff,'tag','difffile1');
            Handle_File2 = findobj(hDiff,'tag','difffile2');
            Handle_FileI = findobj(hDiff,'tag',['difffile' cmd(end)]);
            File=get(Handle_File1,'userdata');
            Str=get(Handle_File1,'string');
            %
            NrInList=get(Handle_FileI,'value');
            %
            if ~isempty(cmdargs)
                [FI,FileName,Tp,Otherargs]=qp_fmem('open',cmdargs{:});
            else
                if ~isempty(File)
                    pn = fileparts(File(NrInList).Name);
                else
                    OpenFile=findobj(mfig,'tag','openfile','type','uipushtool');
                    pn=get(OpenFile,'userdata');
                end
                [FI,FileName,Tp,Otherargs]=qp_fmem('opennew',pn);
            end
            %
            if ~isempty(FI)
                NewRecord.QPF=1;
                NewRecord.Name=FileName;
                NewRecord.Data=FI;
                NewRecord.FileType=Tp;
                if isfield(FI,'Options')
                    NewRecord.Options=FI.Options;
                else
                    NewRecord.Options=0;
                end
                NewRecord.Otherargs=Otherargs;
                %
                if isempty(File)
                    Str={abbrevfn(FileName,60)};
                    NrInList=1;
                    File=NewRecord;
                else
                    FileNameList={File.Name};
                    NrInList=strmatch(FileName,FileNameList,'exact');
                    if isempty(NrInList)
                        NrInList=length(File)+1;
                        Str{NrInList}=abbrevfn(FileName,60);
                        File(NrInList)=NewRecord;
                    end
                end
                set(Handle_File1,'userdata',File);
                set([Handle_File1 Handle_File2],'string',Str,'enable','on','backgroundcolor',Active);
                d3d_qp(['difffile' cmd(end)],NrInList)
            end
            
        case 'difflabel'
            hDiff = findall(0,'tag','Diff Files');
            if length(hDiff)~=1, return, end
            Handle_File1 = findobj(hDiff,'tag','difffile1');
            File = get(Handle_File1,'userdata');
            NrInList1 = get(Handle_File1,'value');
            Handle_File2 = findobj(hDiff,'tag','difffile2');
            NrInList2 = get(Handle_File2,'value');
            Handle_Label = findobj(hDiff,'tag','difflabel');
            Handle_Define = findobj(hDiff,'tag','diffdefine');
            if ~isempty(cmdargs)
                setappdata(Handle_Label,'LabelMode','manual')
                setappdata(Handle_Label,'Label',cmdargs{1})
                set(Handle_Label,'String',cmdargs{1})
            end
            if NrInList1 == NrInList2
                if strcmp(get(Handle_Label,'enable'),'on')
                    set(Handle_Label,'userdata',get(Handle_Label,'string'))
                end
                set(Handle_Label,'String','Data File Equals Reference File', ...
                    'BackgroundColor',Inactive, ...
                    'Enable','off')
                set(Handle_Define,'Enable','off')
            else
                if isequal(gcbo,Handle_Label)
                    % user edited the label
                    Label = get(Handle_Label,'string');
                    LabelRef = getappdata(Handle_Label,'Label');
                    if ~isequal(Label,LabelRef)
                        setappdata(Handle_Label,'LabelMode','manual')
                        setappdata(Handle_Label,'Label',Label)
                    end
                else
                    if isequal(getappdata(Handle_Label,'LabelMode'),'auto')
                        File1 = File(NrInList1).Name;
                        Sep1 = [0 sort([strfind(File1,'/') strfind(File1,'\')]) length(File1)+1];
                        File2 = File(NrInList2).Name;
                        Sep2 = [0 sort([strfind(File2,'/') strfind(File2,'\')]) length(File2)+1];
                        for i=1:min(length(Sep1),length(Sep2))-1
                            if ~isequal(File1(Sep1(i)+1:Sep1(i+1)-1),File2(Sep2(i)+1:Sep2(i+1)-1))
                                break
                            end
                        end
                        for j=1:min(length(Sep1),length(Sep2))-1
                            if ~isequal(File1(Sep1(end-j)+1:Sep1(end-j+1)-1),File2(Sep2(end-j)+1:Sep2(end-j+1)-1))
                                break
                            end
                        end
                        Pre = File1(1:Sep1(i));
                        Post = File1(Sep1(end-j+1):end);
                        Mid1 = File1(Sep1(i)+1:min(end,Sep1(end-j+1)));
                        Mid2 = File2(Sep2(i)+1:min(end,Sep2(end-j+1)));
                        if isempty(Mid1)
                            Mid1 = '.';
                        elseif isempty(Mid2)
                            Mid2 = '.';
                        end
                        if ~isempty(Post) && ~isempty(Pre)
                            Label = [Pre '[(' Mid1 ') - (' Mid2 ')]' Post];
                        elseif ~isempty(Pre)
                            Label = [Pre '[(' Mid1 ') - (' Mid2 ')]'];
                        elseif ~isempty(Post)
                            Label = ['[(' Mid1 ') - (' Mid2 ')]' Post];
                        else
                            Label = ['(' Mid1 ') - (' Mid2 ')'];
                        end
                        set(Handle_Label,'String',Label)
                        setappdata(Handle_Label,'Label',Label)
                    else
                        Label = getappdata(Handle_Label,'Label');
                        set(Handle_Label,'String',Label)
                    end
                end
                set(Handle_Label, ...
                    'BackgroundColor',Active, ...
                    'Enable','on')
                set(Handle_Define,'Enable','on')
            end
            
        case {'difffile1','difffile2'}
            hDiff = findall(0,'tag','Diff Files');
            if length(hDiff)~=1, return, end
            Handle_File1 = findobj(hDiff,'tag','difffile1');
            File=get(Handle_File1,'userdata');
            %
            Handle_SelectFile=findobj(hDiff,'tag',cmd);
            Str=get(Handle_SelectFile,'string');
            %
            if nargin==2
                NrInList = cmdargs{1};
                if ischar(NrInList)
                    if isstruct(File)
                        Names = {File.Name}';
                    else
                        Names = {};
                    end
                    NrInList = ustrcmpi(cmdargs{1},Names);
                    if NrInList<0
                        d3d_qp(['openfile' cmd(end)],cmdargs{1})
                        return
                    else
                        set(Handle_SelectFile,'value',NrInList)
                    end
                elseif isequal(size(NrInList),[1 1]) && NrInList<=length(File) && NrInList>=1 && NrInList==round(NrInList)
                    set(Handle_SelectFile,'value',NrInList)
                else
                    NrInList=get(Handle_SelectFile,'value');
                end
            else
                NrInList=get(Handle_SelectFile,'value');
            end
            switch cmd
                case 'difffile1'
                    SelA = 'Select data file A';
                case 'difffile2'
                    SelA = 'Select reference data file B';
            end
            set(Handle_SelectFile, ...
                'tooltip', [SelA char(10) ...
                ' ' char(10) ...
                'Currently selected:' char(10) ...
                File(NrInList).Name char(10) ...
                'Type: ' File(NrInList).FileType])
            d3d_qp difflabel
            
        case 'diffcancel'
            hDiff = findall(0,'tag','Diff Files');
            if length(hDiff)~=1, return, end
            set(hDiff,'visible','off')
            
        case 'diffdefine'
            hDiff = findall(0,'tag','Diff Files');
            if length(hDiff)~=1, return, end
            Handle_File1 = findobj(hDiff,'tag','difffile1');
            NrInList1 = get(Handle_File1,'value');
            Handle_File2 = findobj(hDiff,'tag','difffile2');
            NrInList2 = get(Handle_File2,'value');
            set(hDiff,'userdata',[NrInList1 NrInList2]);
            set(hDiff,'visible','off')
            
        case {'closefile','closeallfiles'}
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            Str=get(Handle_SelectFile,'string');
            NrInList=get(Handle_SelectFile,'value');
            if strcmp(cmd,'closeallfiles')
                File(:)=[];
                Str(:)=[];
            elseif ~isempty(File)
                File(NrInList)=[];
                Str(NrInList)=[];
            end
            if isempty(File)
                set(Handle_SelectFile,'enable','off','backgroundcolor',Inactive);
                Handle_ReloadFile=findobj(mfig,'tag','reloadfile');
                set(Handle_ReloadFile,'enable','off');
                Handle_CloseFile=findobj(mfig,'tag','closefile');
                set(Handle_CloseFile,'enable','off');
                Handle_CloseAllFiles=findobj(mfig,'tag','closeallfiles');
                set(Handle_CloseAllFiles,'enable','off');
                set(findobj(mfig,'tag','fileinfo'),'enable','off');
                Str=' ';
                NrInList=1;
            else
                NrInList=min(NrInList,length(File));
            end
            set(Handle_SelectFile,'userdata',File,'string',Str,'value',NrInList);
            d3d_qp selectfile*
            if logfile
                writelog(logfile,logtype,cmd);
            end
            
        case 'selectfile'
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            if ~isempty(File)
                if isempty(cmdargs)
                    NrInList=get(Handle_SelectFile,'value');
                else
                    Files={File.Name};
                    NrInList=ustrcmpi(cmdargs{1},Files);
                    if NrInList<0
                        error(sprintf('Cannot select file: %s',cmdargs{1}))
                    else
                        set(Handle_SelectFile,'value',NrInList);
                    end
                end
                if logfile
                    writelog(logfile,logtype,cmd,File(NrInList).Name);
                end
            end
            d3d_qp selectfile*
            
        case 'selectfile*'
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            
            Handle_FO=findobj(mfig,'tag','fileoptions');
            
            File=get(Handle_SelectFile,'userdata');
            OptionsControls=setdiff(allchild(UD.FilOpt.Fig),UD.FilOpt.Close);
            if isempty(File)
                
                set(Handle_FO,'enable','off','state','off')
                delete(OptionsControls)
                set(UD.FilOpt.Fig,'visible','off')
                
            else
                NrInList=get(Handle_SelectFile,'value');
                
                delete(OptionsControls)
                if File(NrInList).Options
                    set(Handle_FO,'enable','on')
                    Chk=qp_getdata(File(NrInList),'options',UD.FilOpt.Fig,'initialize');
                else
                    set(Handle_FO,'enable','off','state','off')
                    set(UD.FilOpt.Fig,'visible','off')
                end
                set(Handle_SelectFile,'tooltip', ...
                    ['Select a data file for plotting' char(10) ...
                    ' ' char(10) ...
                    'Currently selected:' char(10) ...
                    File(NrInList).Name char(10) ...
                    'Type: ' File(NrInList).FileType])
                
                OpenFile=findobj(mfig,'tag','openfile','type','uipushtool');
                Handle_ReloadFile=findobj(mfig,'tag','reloadfile');
                Handle_CloseFile=findobj(mfig,'tag','closefile');
                Handle_CloseAllFiles=findobj(mfig,'tag','closeallfiles');
                switch File(NrInList).FileType
                    case '<user defined variables>'
                        set(Handle_ReloadFile,'enable','off');
                        set(Handle_CloseFile,'enable','off');
                    case 'diff'
                        set(Handle_ReloadFile,'enable','on');
                        set(Handle_CloseFile,'enable','on');
                    otherwise
                        pn = fileparts(File(NrInList).Name);
                        set(OpenFile,'userdata',pn);
                        set(Handle_ReloadFile,'enable','on');
                        set(Handle_CloseFile,'enable','on');
                end
            end
            d3d_qp updatedomains
            
        case 'fileinfo'
            Fil=qpfile;
            ui_inspectstruct(Fil.Data,Fil.Name)
            
        case 'fileoptions'
            if nargin==1
                if strcmp(get(UD.FilOpt.Fig,'visible'),'on')
                    set(UD.FilOpt.Fig,'visible','off')
                    set(UD.MainWin.FlOps,'state','off')
                else
                    set(UD.FilOpt.Fig,'visible','on')
                    set(UD.MainWin.FlOps,'state','on')
                end
            else
                Handle_SelectFile=findobj(mfig,'tag','selectfile');
                File=get(Handle_SelectFile,'userdata');
                NrInList=get(Handle_SelectFile,'value');
                [Chk,NewFileInfo,cmdargs]=qp_getdata(File(NrInList),'options',UD.FilOpt.Fig,varargin{:});
                if Chk && ~isequal(NewFileInfo,[]),
                    File(NrInList)=NewFileInfo;
                    set(Handle_SelectFile,'userdata',File,'value',NrInList);
                    d3d_qp updatedomains
                end
                if ~isempty(cmdargs)
                    if logfile
                        writelog(logfile,logtype,cmd,cmdargs{:});
                    end
                end
            end
            
            
        case 'updatedomains'
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            NrInList=get(Handle_SelectFile,'value');
            Succes=~isempty(File);
            if Succes
                Info=File(NrInList);
            end
            
            domaintxt=findobj(mfig,'tag','domain');
            domains=findobj(mfig,'tag','selectdomain');
            if ~Succes
                set(domains,'string',' ','value',1,'enable','off','backgroundcolor',Inactive);
                set(domaintxt,'enable','off');
            else
                [Chk,Domains]=qp_getdata(Info,'domains');
                if ~Chk || isempty(Domains)
                    set(domains,'string',' ','value',1,'enable','off','backgroundcolor',Inactive);
                    set(domaintxt,'enable','off');
                else
                    dm=length(Domains);
                    if strcmp(get(domains,'enable'),'on')
                        pdomains=get(domains,'string');
                        dm=get(domains,'value');
                        pdomain=pdomains{dm};
                        dm=ustrcmpi(pdomain,Domains); % first check equality and longer names
                        if dm<0 % no matches
                            dm=ustrcmpi(Domains,pdomain); % check (equality and) shorter names
                            if dm<0 % still no matches
                                dm=length(Domains);
                            end
                        end
                    end
                    set(domains,'string',Domains,'value',dm,'enable','on','backgroundcolor',Active);
                    set(domaintxt,'enable','on');
                end
            end
            d3d_qp updatedatafields
            
        case 'selectdomain'
            domains=findobj(mfig,'tag','selectdomain');
            Domains=get(domains,'string');
            if ~isempty(cmdargs)
                i=ustrcmpi(cmdargs{1},Domains);
                if i<0
                    error(sprintf('Cannot select field: %s',cmdargs{1}))
                else
                    set(domains,'value',i);
                end
            end
            d3d_qp updatedatafields
            if logfile
                writelog(logfile,logtype,cmd,Domains{get(domains,'value')});
            end
            
        case 'updatedatafields'
            set(mfig,'pointer','watch')
            Props=[];
            lasterr('');
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            NrInList=get(Handle_SelectFile,'value');
            Succes=~isempty(File);
            if Succes
                Info=File(NrInList);
            end
            
            datafields=findobj(mfig,'tag','selectfield');
            if ~Succes
                set(datafields,'string',' ','value',1,'enable','off','backgroundcolor',Inactive,'userdata',Props);
                set(mfig,'pointer','arrow')
                d3d_qp updatefieldprop
            else
                Handle_Domain=findobj(mfig,'tag','selectdomain');
                DomainNr=get(Handle_Domain,'value');
                %
                [Chk,Props]=qp_getdata(Info,DomainNr);
                if ~Chk
                    set(datafields, ...
                        'string',sprintf('<%s not supported>',qp_gettype(Info)), ...
                        'value',1, ...
                        'enable','off', ...
                        'backgroundcolor',Inactive, ...
                        'userdata',Props);
                    set(mfig,'pointer','arrow')
                    d3d_qp updatefieldprop
                else
                    names={Props.Name};
                    if isempty(names),
                        set(datafields,'string','<no datafields found>','value',1,'enable','off','backgroundcolor',Inactive,'userdata',Props);
                        set(mfig,'pointer','arrow')
                        d3d_qp updatefieldprop
                    else
                        df=1;
                        if strcmp(get(datafields,'enable'),'on')
                            pnames=get(datafields,'string');
                            df=get(datafields,'value');
                            pname=pnames{df};
                            df=ustrcmpi(pname,names); % first check equality and longer names
                            if df<0, % no matches
                                df=ustrcmpi(names,pname); % check (equality and) shorter names
                                if df<0, % still no matches
                                    df=1;
                                end
                            end
                        end
                        set(datafields,'string',names,'value',df,'enable','on','backgroundcolor',Active,'userdata',Props);
                    end
                end
            end
            d3d_qp updatefieldprop
            set(mfig,'pointer','arrow')
            
        case {'selectfield','selectsubfield'}
            sf=findobj(mfig,'tag',cmd);
            flds=get(sf,'string');
            if ~isempty(cmdargs)
                i=ustrcmpi(cmdargs{1},flds);
                if i<0
                    error(sprintf('Cannot select %s: %s',cmd(7:end),cmdargs{1}))
                else
                    set(sf,'value',i);
                end
            end
            d3d_qp updatefieldprop
            if logfile
                writelog(logfile,logtype,cmd,flds{get(sf,'value')});
            end
            
        case 'updatefieldprop'
            qp_updatefieldprop(UD);
            d3d_qp('gridview_update')
            
        case 'showtimeswitch'
            UDSts=findobj(mfig,'tag','showtimes');
            if isempty(cmdargs)
                Sts=get(UDSts,'value');
            else
                Sts=cmdargs{1};
                if ~isequal(Sts,0) && ~isequal(Sts,1)
                    error(sprintf('Invalid argument specified for %s.',cmd))
                end
                set(UDSts,'value',Sts);
            end
            d3d_qp showtimes
            if logfile
                writelog(logfile,logtype,cmd,get(UDSts,'value'));
            end
            
        case 'showtimes'
            st=findobj(mfig,'tag','showtimes');
            tl=findobj(mfig,'tag','timelist');
            if get(st,'value') && strcmp(get(st,'enable'),'on')
                if ~get(tl,'userdata')
                    datafields=findobj(mfig,'tag','selectfield');
                    fld=get(datafields,'value');
                    Props=get(datafields,'userdata');
                    
                    Handle_SelectFile=findobj(mfig,'tag','selectfile');
                    File=get(Handle_SelectFile,'userdata');
                    NrInList=get(Handle_SelectFile,'value');
                    Succes=~isempty(File);
                    if Succes
                        Info=File(NrInList);
                    end
                    
                    Handle_Domain=findobj(mfig,'tag','selectdomain');
                    DomainNr=get(Handle_Domain,'value');
                    
                    set(mfig,'pointer','watch')
                    [Chk,Times]=qp_getdata(Info,DomainNr,Props(fld),'times');
                    set(mfig,'pointer','arrow')
                    if size(Times,1)==1
                        Times=Times';
                    end
                    nTimes=length(Times);
                    switch Props(fld).DimFlag(T_)
                        case {1,2} % day-month-year h:m:s
                            % 1: discrete, 2: continuous
                            Str=datestr(Times,0);
                        case {3,4} % day h:m:s
                            % 3: discrete, 4: continuous
                            Str=cat(2,repmat('day ',nTimes,1),num2str(floor(Times)), ...
                                repmat(' ',nTimes,1),datestr(rem(Times,1),13));
                        case {5,6,0} % i
                            % 5: discrete, 6: continuous
                            Str=num2str(Times);
                        case {7,8} % seconds
                            % 7: discrete, 8: continuous
                            Str=cat(2,num2str(Times*24*3600),repmat(' s',nTimes,1));
                    end
                    set(tl,'string',Str,'userdata',1)
                end
                id=get(findobj(mfig,'tag','t=?'),'userdata');
                set(tl,'value',id,'backgroundcolor',Active,'enable','on')
            else
                set(tl,'value',[],'backgroundcolor',Inactive,'enable','off')
            end
            
        case 'station'
            sts=get(UD.MainWin.StList,'string');
            if ~isempty(cmdargs)
                if isnumeric(cmdargs{1}) && isequal(size(cmdargs{1}),[1 1])
                    i = cmdargs{1};
                    nsts = max(size(sts,1),length(sts));
                    if i<1 || i>nsts
                        ui_message('warning', ...
                            sprintf('Requested station number %i bigger than number of stations %i.',i,nsts));
                        i = get(UD.MainWin.StList,'value');
                    else
                        i = cmdargs{1};
                    end
                else
                    stn=deblank(cmdargs{1});
                    [i,iall]=ustrcmpi(stn,sts);
                end
                if i<0
                    if length(iall)>1
                        ui_message('warning', ...
                            sprintf('Multiple station names match ''%s'', selecting first',stn));
                        set(UD.MainWin.StList,'value',iall(1));
                    else
                        error(sprintf('Cannot select station: %s',cmdargs{1}))
                    end
                else
                    set(UD.MainWin.StList,'value',i);
                end
            end
            pst=get(UD.MainWin.StList,'value');
            set(UD.MainWin.EditS,'userdata',pst,'string',sprintf('%i',pst))
            statud=get(UD.MainWin.AllS,'userdata');
            statud{2}=pst;
            set(UD.MainWin.AllS,'userdata',statud)
            if logfile
                if iscell(sts)
                    stn=sts{pst};
                else
                    stn=sts(pst,:);
                end
                writelog(logfile,logtype,cmd,stn);
            end
            
        case 'timelist'
            pUDM=get(UD.MainWin.AllT,'userdata');
            id=get(UD.MainWin.TList,'value');
            set(UD.MainWin.EditT,'string',vec2str(id,'noones','nobrackets'),'userdata',id);
            pUDM{2}=id;
            set(UD.MainWin.AllT,'userdata',pUDM);
            d3d_qp updateoptions
            % do as if the values were entered in the editt edit box
            if logfile
                writelog(logfile,logtype,'editt',id);
            end
            
        case {'allm','alln','allk','allt','alls','allm*','alln*','allk*','allt*','alls*'}
            M=upper(cmd(4));
            MW=UD.MainWin;
            UDAllM=getfield(MW,strcat('All',M));
            UDEditM=getfield(MW,strcat('Edit',M));
            pUDM=get(UDAllM,'userdata');
            %
            if M=='M' || M=='N'
                if ~strcmp(getvalstr(MW.HSelType),'M range and N range')
                    d3d_qp('m,n*')
                end
            end
            %
            if isempty(cmdargs)
                SwitchToAll=get(UDAllM,'value');
            else
                SwitchToAll=cmdargs{1};
                if isequal(SwitchToAll,'0') || isequal(SwitchToAll,'1')
                    SwitchToAll = str2num(SwitchToAll);
                elseif ~isequal(SwitchToAll,0) && ~isequal(SwitchToAll,1)
                    error(sprintf('Invalid argument specified for %s.',cmd))
                end
                set(UDAllM,'value',SwitchToAll);
            end
            if SwitchToAll
                pUDM{1}=1;
                set(UDEditM,'enable','off','backgroundcolor',Inactive)
                if M=='S'
                    set(findobj(mfig,'tag','stationlist'),'enable','off','backgroundcolor',Inactive)
                end
            else
                pUDM{1}=0;
                set(UDEditM,'enable','on','backgroundcolor',Active)
                if M=='S'
                    set(findobj(mfig,'tag','stationlist'),'enable','on','backgroundcolor',Active)
                elseif M=='T'
                    set(findobj(mfig,'tag','quickview'),'string','Quick View');
                end
            end
            set(UDAllM,'userdata',pUDM);
            %----------
            if M=='T'
                if SwitchToAll
                    set(MW.ShowT,'enable','off')
                    d3d_qp showtimes
                else
                    UDMaxM=getfield(MW,strcat('Max',M));
                    maxm=get(UDMaxM,'userdata');
                    if maxm<=30000 % enable timelist only if there are less than 30000 times
                        set(MW.ShowT,'enable','on')
                        d3d_qp showtimes
                    end
                end
            end
            d3d_qp updateoptions
            if logfile
                writelog(logfile,logtype,cmd(1:4),SwitchToAll);
            end
            
            if ~strcmp(cmd(end),'*')
                d3d_qp('gridview_update');
            end
            
        case {'editm','editn','editk','editt','edits','editm*','editn*','editk*','editt*','edits*'}
            M=upper(cmd(5));
            MW=UD.MainWin;
            UDAllM=getfield(MW,strcat('All',M));
            UDEditM=getfield(MW,strcat('Edit',M));
            UDMaxM=getfield(MW,strcat('Max',M));
            maxm=get(UDMaxM,'userdata');
            pUDM=get(UDAllM,'userdata');
            
            try
                if isempty(cmdargs)
                    mstr=get(UDEditM,'string');
                else
                    mstr=cmdargs{1};
                end
                if ischar(mstr)
                    m=str2vec(mstr,'range',[1 maxm],'applylimit');
                else
                    m=mstr;
                end
            catch
                lasterr('')
                m=1;
            end
            if isempty(m)
                m=1;
            end
            if M~='T'
                %
                m_=find('TSMNK'==M);
                if m_==3 || m_==4
                    if ~strcmp(getvalstr(MW.HSelType),'M range and N range')
                        d3d_qp('m,n*')
                    end
                end
                datafields=findobj(mfig,'tag','selectfield');
                fld=get(datafields,'value');
                Props=get(datafields,'userdata');
                DimFlag=Props(fld).DimFlag;
                %
                % often only subranges are acceptable: m1:m2, not m1:mi:m2 or [m1 m2 m4]
                %
                if ~isequal(m,m(1):m(end)) && DimFlag(m_)~=6 && DimFlag(m_)~=16
                    m=m(1);
                end
                switch DimFlag(m_)
                    case {1,11} % All, Range, Element
                        % nothing to do
                    case {2,12} % All, Range
                        if isequal(size(m),[1 1])
                            if m==1
                                m=1:maxm;
                                
                            else
                                m=1:m;
                            end
                        end
                    case {3,13} % All, Element
                        m=m(1);
                    case {4,14,inf} % All
                        % cannot occur
                    case {5,15} % Element
                        m=m(1);
                    case {6,16}
                        m=unique(m);
                end
                %
            else
                m=unique(m);
                st=findobj(mfig,'tag','showtimes');
                tl=findobj(mfig,'tag','timelist');
                if get(MW.ShowT,'value') && strcmp(get(MW.ShowT,'enable'),'on')
                    set(MW.TList,'value',m);
                end
            end
            set(UDEditM,'string',vec2str(m,'nobrackets','noones'),'userdata',m);
            pUDM{2}=m;
            set(UDAllM,'userdata',pUDM);
            d3d_qp updateoptions
            if logfile
                writelog(logfile,logtype,cmd(1:5),m);
            end
            
            if ~strcmp(cmd(end),'*')
                d3d_qp('gridview_update');
            end
            
        case {'editmn*','editmn','editxy*','editxy'}
            MW=UD.MainWin;
            isMN = isequal(cmd(5:6),'mn');
            if isMN
                UDEditMN=MW.EditMN;
                if ~strcmp(getvalstr(MW.HSelType),'(M,N) point/path')
                    d3d_qp('(mn)*')
                end
            else
                UDEditMN=MW.EditXY;
                if ~strcmp(getvalstr(MW.HSelType),'(X,Y) point/path')
                    d3d_qp('(xy)*')
                end
            end
            try
                if isempty(cmdargs)
                    mnstr=get(UDEditMN,'string');
                else
                    mnstr=cmdargs{1};
                end
                if ischar(mnstr)
                    mn=str2num(mnstr);
                    if isempty(mn)
                        if size(mnstr,1)>1
                            mnstr=mnstr';
                            mnstr(end+1,:)=' ';
                            mnstr=mnstr(:)';
                        end
                        mn=sscanf(mnstr,'%g%*[,; ]');
                    end
                else
                    mn=mnstr;
                end
                switch get(UD.MainWin.N,'enable')
                    case 'on'
                        if size(mn,1)==1 && size(mn,2)>2
                            L = length(mn);
                            if L~=2*round(L/2)
                                %ui_message('error','Even number of numbers expected: ignoring last value');
                                mn = mn(1:end-1);
                                L = length(mn);
                            end
                            mn = reshape(mn,[2 L/2])';
                        elseif size(mn,2)==1 && size(mn,1)>=1
                            L = length(mn);
                            if L~=2*round(L/2)
                                %ui_message('error','Even number of numbers expected: ignoring last value');
                                mn = mn(1:end-1);
                                L = length(mn);
                            end
                            mn = reshape(mn,[2 L/2])';
                        elseif size(mn,2)>2
                            %ui_message('error','Two columns expected: ignoring superfluous columns')
                            mn = mn(:,1:2);
                        end
                        dims=2;
                    case 'off'
                        dims=1;
                        if isMN
                            mn=mn(:);
                        end
                end
            catch
                lasterr('')
                mn=[];
            end
            if ~isempty(mn)
                if isMN
                    if any(mn(:)~=round(mn(:)))
                        mn=get(UDEditMN,'userdata');
                    else
                        % clip all indices to range
                        if dims==2
                            % structured 2D domain
                            mmax = get(MW.MaxM,'userdata');
                            nmax = get(MW.MaxN,'userdata');
                            [mnexp,mn]=piecewise(mn,[mmax nmax]);
                        else
                            %unstructured domain
                            mmax = get(MW.MaxM,'userdata');
                            %
                            firstproblem = min(find(any(mn<1 | mn>mmax)));
                            if ~isempty(firstproblem)
                                mn = mn(1:firstproblem-1);
                            end
                        end
                    end
                else
                    mn(any(isnan(mn) | ~isfinite(mn),2),:)=NaN;
                end
            end
            set(UDEditMN,'userdata',mn)
            if isempty(mn)
                mnstr='';
            else
                if isMN
                    if dims==2
                        mnstr=sprintf('%i, %i; ',mn');
                    else
                        mnstr=sprintf('%i; ',mn');
                    end
                else
                    mnstr='';
                    for i=1:size(mn,1)
                        xf = sprintf('%%.%if',min(3,6-floor(log10(abs(mn(i,1))))));
                        yf = sprintf('%%.%if',min(3,6-floor(log10(abs(mn(i,2))))));
                        mnstr=[mnstr sprintf([xf,', ',yf,'; '],mn(i,:))];
                    end
                end
                mnstr(end-1:end)=[];
            end
            set(UDEditMN,'string',mnstr)
            d3d_qp updateoptions
            if logfile
                wrcmd=cmd;
                if cmd(end)=='*'
                    wrcmd=cmd(1:end-1);
                end
                writelog(logfile,logtype,wrcmd,mnstr);
            end
            
            if cmd(end)~='*'
                d3d_qp('gridview_update');
            end
            
        case {'editz'}
            if isempty(cmdargs)
                z=str2num(get(UD.MainWin.EditZ,'string'));
            else
                z=cmdargs{1};
            end
            if isempty(z)
                z=[];
            elseif ~isequal(size(z),[1 1])
                z=z(1);
            end
            set(UD.MainWin.EditZ,'string',sprintf('%g',z),'userdata',z)
            d3d_qp updateoptions
            if logfile
                writelog(logfile,logtype,cmd,z);
            end
            
        case {'defvariable','loaddata','quickview','updateoptions','exportdata','addtoplot'}
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            NrInList=get(Handle_SelectFile,'value');
            Succes=~isempty(File);
            Info=[];
            if Succes
                Info=File(NrInList);
                [DomainNr,Props,subf,selected,stats,Ops]=qp_interface_update_options(mfig,UD);
                if strcmp(Ops.presentationtype,'failed')
                    cmd='error';
                end
            else
                cmd='error';
            end
            
            switch cmd
                case 'error'
                    
                case 'defvariable'
                    Handle_SelectFile=findobj(mfig,'tag','selectfile');
                    File=get(Handle_SelectFile,'userdata');
                    sel=get(Handle_SelectFile,'value');
                    Str=get(Handle_SelectFile,'string');
                    UDV='<user defined variables>';
                    FileName=UDV;
                    FileNameList={File.Name};
                    NrInList=strmatch(FileName,FileNameList,'exact');
                    if ~isempty(NrInList) && ~isempty(File(NrInList).Data)
                        Vars={File(NrInList).Data.Name};
                    else
                        Vars={};
                    end
                    if isempty(cmdargs)
                        VarName=Props.Name;
                        accept=0;
                    else
                        VarName=cmdargs{1};
                        accept=isempty(strmatch(VarName,Vars,'exact'));
                    end
                    prompt={'Name of Variable:'};
                    
                    def={VarName};
                    dlgTitle='Specify Unique Name of Variable';
                    lineNo=1;
                    if ~isempty(strmatch(VarName,Vars,'exact'))
                        prompt={'Name of Variable (the Current Name is Not Unique):'};
                    end
                    while ~accept
                        answer=stdinputdlg(prompt,dlgTitle,lineNo,def);
                        if isempty(answer)
                            break
                        end
                        VarName=answer{1};
                        accept=isempty(strmatch(VarName,Vars,'exact'));
                    end
                    if accept
                        if isempty(NrInList)
                            NrInList=length(File)+1;
                            File(NrInList).Data=[];
                        end
                        Str{NrInList}=abbrevfn(FileName,60);
                        File(NrInList).QPF=1;
                        File(NrInList).Name=FileName;
                        if isempty(File(NrInList).Data)
                            i=1;
                        else
                            i=length(File(NrInList).Data)+1;
                        end
                        File(NrInList).Data(i).Name=VarName;
                        File(NrInList).Data(i).FileInfo=Info;
                        File(NrInList).Data(i).Domain=DomainNr;
                        File(NrInList).Data(i).Props=Props;
                        File(NrInList).Data(i).Selected=selected;
                        File(NrInList).Data(i).SubField=subf;
                        File(NrInList).Data(i).DimFlag=Props.DimFlag;
                        File(NrInList).Data(i).DataInCell=0;
                        if isfield(Props,'DataInCell')
                            File(NrInList).Data(i).DataInCell=Props.DataInCell;
                        end
                        File(NrInList).Data(i).Tri=0;
                        if isfield(Props,'Geom') && isequal(Props.Geom,'TRI')
                            File(NrInList).Data(i).Tri=1;
                        elseif isfield(Props,'Tri')
                            File(NrInList).Data(i).Tri=Props.Tri;
                        end
                        File(NrInList).FileType='<user defined variables>';
                        File(NrInList).Options=1;
                        set(Handle_SelectFile,'userdata',File,'string',Str,'enable','on','backgroundcolor',Active);
                        if isequal(Str{sel},UDV)
                            d3d_qp selectfile*
                        end
                        if logfile
                            writelog(logfile,logtype,cmd,VarName);
                        end
                    end
                case 'exportdata'
                    lasterr('');
                    try
                        if isempty(cmdargs)
                            FileName='';
                        else
                            FileName=cmdargs{1};
                        end
                        set(mfig,'pointer','watch')
                        if isfield(Ops,'MNK') && Ops.MNK
                            Props.MNK=1.5;
                        end
                        DS.FI=Info;
                        DS.Domain=DomainNr;
                        DS.Props=Props;
                        DS.SubField=subf;
                        DS.Selected=selected;
                        DS.Ops=Ops;
                        FileName=qp_export(Ops.exporttype,FileName,DS);
                        set(mfig,'pointer','arrow')
                    catch
                        FileName='';
                        set(mfig,'pointer','arrow')
                        ui_message('error',{'Catch in d3d_qp\exportdata',lasterr})
                    end
                    if ~isempty(FileName) && logfile
                        writelog(logfile,logtype,cmd,FileName);
                    end
                    
                case 'loaddata'
                    if logfile
                        writelog(logfile,logtype,cmd);
                    end
                    lasterr('');
                    try
                        selected(~Props.DimFlag)=[];
                        set(mfig,'pointer','watch')
                        if Ops.MNK
                            Props.MNK=1.5;
                        end
                        switch Ops.presentationtype
                            case {'patches','patches with lines'}
                                [Chk,data,Info]=qp_getdata(Info,DomainNr,Props,'gridcelldata',subf{:},selected{:});
                            otherwise
                                [Chk,data,Info]=qp_getdata(Info,DomainNr,Props,'griddata',subf{:},selected{:});
                        end
                        % update FileInfo ...
                        File(NrInList)=Info;
                        set(Handle_SelectFile,'userdata',File);
                        % reset pointer and return data ...
                        set(mfig,'pointer','arrow')
                        if nargout>0
                            outdata=data;
                        else
                            assignin('base','data',data)
                        end
                    catch
                        set(mfig,'pointer','arrow')
                        ui_message('error',{'Catch in d3d_qp\loaddata',lasterr})
                    end
                    
                case {'quickview','addtoplot'},
                    if logfile
                        writelog(logfile,logtype,cmd);
                    end
                    hNew=[];
                    %
                    % animate time dimension when appropriate ...
                    %
                    T=1;
                    Animate=Ops.animate;
                    if Animate && Props.DimFlag(T_)
                        if selected{T_}==0
                            maxt=get(findobj(mfig,'tag','max_t'),'userdata');
                            T=1:maxt;
                        else
                            T=selected{T_};
                        end
                        selected{T_}=T(1);
                    end
                    
                    if strcmp(cmd,'quickview')
                        UDplot.ProgID='QuickPlot';
                        
                        pfig=qp_createfig('quick','');
                        ax=findall(pfig,'type','axes');
                        for i=length(ax):-1:1
                            if isappdata(ax(i),'NonDataObject')
                                ax(i)=[];
                            elseif isappdata(ax(i),'AxesType')
                                ax(i)=[];
                            end
                        end
                        if isempty(ax)
                            Parent=axes('color',qp_settings('defaultaxescolor')/255);
                            if qp_settings('boundingbox')
                                set(Parent,'box','on');
                            end
                        else
                            Parent=ax(1);
                        end
                    else
                        Parent=UD.PlotMngr.CurrentAxes;
                        if ishandle(Parent),
                            pfig=get(Parent,'parent');
                        else
                            pfig=[];
                            Parent=[];
                        end
                    end
                    
                    if ishandle(Parent)
                        set(mfig,'pointer','watch')
                        try
                            PS.FI=Info;
                            
                            PS.Domain=DomainNr;
                            PS.Props=Props;
                            PS.SubField=subf;
                            PS.Selected=selected;
                            PS.Parent=Parent;
                            PS.Handles=hNew;
                            PS.Stations=stats;
                            PS.Ops=Ops;
                            [hNew,Error,Info]=qp_plot(PS);
                        catch
                            ui_message('error',{'Catch in d3d_qp\quickview',lasterr})
                        end
                        set(mfig,'pointer','arrow')
                    end
                    
                    if ~isempty(pfig)
                        set(UD.PlotMngr.FigList,'value',1,'string',listnames(pfig,'showType','no','showHandle','no','showTag','no'),'userdata',pfig);
                        d3d_qp refreshfigs
                    end
                    qp_updatescroller(hNew,pfig)
                    
                    if Animate
                        qck_anim('start',pfig,T);
                    end
            end
            d3d_qp update_addtoplot
            
        case {'colourmapeditor'}
            Hcmap=findobj(UOH,'tag','colourmap=?');
            cmaps=get(Hcmap,'string');
            cmapname=cmaps{get(Hcmap,'value')};
            cmapstruct=qp_colormap(':getstruct',cmapname);
            uicontrolfont = qp_fontsettings('DefaultUicontrolFont');
            cmapstruct=md_colormap(cmapstruct,uicontrolfont);
            cmaps=qp_colormap(':reload');
            newcmapname=cmapstruct.Name;
            i=ustrcmpi(newcmapname,cmaps);
            if i<0
                i=ustrcmpi(cmapname,cmaps);
                if i<0
                    i=1;
                end
            end
            set(Hcmap,'value',1,'string',cmaps,'value',i);
            %
            d3d_qp colourmap*
            %
            if logfile
                writelog(logfile,logtype,'colourmap',cmaps{get(Hcmap,'value')});
            end
            
        case {'colourmap','colourmap*'}
            % command requires an input string
            Hcmaplist=findobj(UOH,'tag','colourmap=?');
            cmaps=get(Hcmaplist,'string');
            if ~isempty(cmdargs)
                i=ustrcmpi(cmdargs{1},cmaps);
                if i<0
                    error(sprintf('Invalid %s: %s',cmd,cmdargs{1}))
                else
                    set(Hcmaplist,'value',i);
                end
            end
            cmapsel=get(Hcmaplist,'value');
            cmapprev=findobj(UOH,'tag','colourmapbutton');
            pos=get(cmapprev,'pos');
            reqcmapl=pos(3);
            cmaps=get(Hcmaplist,'string');
            cmap=qp_colormap(cmaps{cmapsel},reqcmapl);
            cmaplength=size(cmap,1);
            cmap=cmap(floor(cmaplength*(0:(reqcmapl-1))/reqcmapl)+1,:);
            set(cmapprev,'cdata',repmat(reshape(cmap,[1 size(cmap)]),18,1))
            if logfile && cmd(end)~='*'
                writelog(logfile,logtype,cmd,cmaps{get(Hcmaplist,'value')});
            end
            
        case {'selectiontype','axestype','plotcoordinate','component','plottype', ...
                'climmode','presenttype','vecscalem','vertscalem','thinfld', ...
                'linestyle','marker','threshdistr','horizontalalignment', ...
                'verticalalignment','exporttype','vectorcolour','dataunits', ...
                'vectorstyle'}
            % commands require an input string
            %
            % nothing to do except refreshing the options
            if strcmp(cmd,'plottype')
                cmd='component';
            end
            modelist=findobj(UOH,'tag',[cmd '=?']);
            modes=get(modelist,'string');
            if ~isempty(cmdargs)
                i=ustrcmpi(cmdargs{1},modes);
                if i<0
                    if strcmp(cmd,'exporttype') && strcmp(cmdargs{1},'mat file')
                        %
                        % convert 'mat file' export type to 'mat file (v6)' for
                        % consistency
                        %
                        i=ustrcmpi('mat file (v6)',modes);
                        if i<0
                            error(sprintf('Invalid %s: %s',cmd,cmdargs{1}))
                        else
                            set(modelist,'value',i);
                        end
                    elseif strcmp(cmd,'dataunits')
                        %
                        % if it is not a mode string, it must be interpreted as a unit
                        % string.
                        %
                        modelist=findobj(UOH,'tag',[cmd '=!']);
                        set(modelist,'string',cmdargs{1})
                    else
                        error(sprintf('Invalid %s: %s',cmd,cmdargs{1}))
                    end
                else
                    set(modelist,'value',i);
                end
            end
            d3d_qp updateoptions
            if logfile
                writelog(logfile,logtype,cmd,modes{get(modelist,'value')});
            end
            
        case {'colour','facecolour','markercolour','markerfillcolour','textboxfacecolour', ...
                'textboxcolour'}
            % commands require an input color
            %
            switch cmd
                case 'textboxcolour'
                    cmd='textboxfacecolour';
            end
            clrh=findobj(UOH,'tag',[cmd '=?']);
            if isempty(cmdargs)
                clr=uisetcolor(get(clrh,'userdata'),'Specify the colour ...');
            else
                clr=cmdargs{1};
            end
            if ischar(clr)
                switch clr
                    case 'r'
                        clr=[1 0 0];
                    case 'g'
                        clr=[0 1 0];
                    case 'b'
                        clr=[0 0 1];
                    case 'c'
                        clr=[0 1 1];
                    case 'm'
                        clr=[1 0 1];
                    case 'y'
                        clr=[1 1 0];
                    case 'k'
                        clr=[0 0 0];
                    case 'w'
                        clr=[1 1 1];
                    otherwise
                        clr=str2vec(clr,'%f');
                end
            end
            if isequal(size(clr),[1 3]) && all(clr>=0 & clr<=1)
                set(clrh,'backgroundcolor',clr,'userdata',clr)
            elseif ~isempty(cmdargs)
                error('Invalid colour specification.');
            end
            if logfile
                writelog(logfile,logtype,cmd,clr);
            end
            
        case {'colourvectors','usemarkercolour','usemarkerfillcolour','colclassify','colourbar','colourdams','textbox','fillpolygons', ...
                'colvector','coldams'}
            % commands require an input logical
            %
            % nothing do except refreshing the options
            switch cmd
                case 'coldams'
                    cmd='colourdams';
                case 'colvector'
                    cmd='colourvectors';
            end
            tag=cmd;
            switch cmd
                case 'textbox'
                    tag='textbox=?';
            end
            cb=findobj(UOH,'tag',tag);
            if ~isempty(cmdargs)
                Log=cmdargs{1};
                if ~isequal(Log,0) && ~isequal(Log,1)
                    error(sprintf('Invalid argument specified for %s.',cmd))
                end
                set(cb,'value',Log);
            end
            d3d_qp updateoptions
            if logfile
                writelog(logfile,logtype,cmd,get(cb,'value'));
            end
            
        case {'colbarhorz','climsymm','extend2edge'}
            % nothing do
            cb=findobj(UOH,'tag',cmd);
            if ~isempty(cmdargs)
                Log=cmdargs{1};
                if ~isequal(Log,0) && ~isequal(Log,1)
                    error(sprintf('Invalid argument specified for %s.',cmd))
                end
                set(cb,'value',Log);
            end
            if logfile
                writelog(logfile,logtype,cmd,get(cb,'value'));
            end
            
        case {'numformat'}
            cv=findobj(UOH,'tag','numformat=?');
            if isempty(cmdargs)
                Str=get(cv,'string');
            else
                Str=cmdargs{1};
            end
            set(cv,'string',Str)
            if logfile
                writelog(logfile,logtype,cmd,Str);
            end
            
        case {'thresholds'}
            cv=findobj(UOH,'tag',[cmd '=?']);
            if isempty(cmdargs)
                Str=get(cv,'string');
            else
                Str=cmdargs{1};
            end
            if ischar(Str)
                c=unique(str2vec(Str,'%f'));
            else
                c=Str;
            end
            set(cv,'string',vec2str(c,'noones','nobrackets'),'userdata',c)
            if strcmp(cmd,'thresholds')
                d3d_qp updateoptions
            end
            if logfile
                writelog(logfile,logtype,cmd,c);
            end
            
        case {'xclipping','yclipping','clippingvals'}
            cv=findobj(UOH,'tag',[cmd '=?']);
            if isempty(cmdargs)
                Str=get(cv,'string');
            else
                Str=cmdargs{1};
            end
            lasterr('');
            try
                if ischar(Str)
                    [c,Str]=realset(Str);
                else
                    c=Str;
                    Str=realset(c);
                end
            catch
                ui_message('error',{['Catch in d3d_qp\' cmd],lasterr})
                c=get(cv,'userdata');
                if isstruct(c)
                    Str=realset(c);
                else
                    Str=vec2str(c,'noones','nobrackets');
                end
            end
            set(cv,'string',Str,'userdata',c)
            if strcmp(cmd,'thresholds')
                d3d_qp updateoptions
            end
            if logfile
                writelog(logfile,logtype,cmd,Str);
            end
            
        case 'optslider'
            os=UD.Options.Slider;
            offset=get(os,'value');
            act=UD.Options.Act;
            p=UD.Options.ActPos;
            p(:,2)=p(:,2)-offset;
            p=num2cell(p,2);
            set(UOH(act)',{'position'},p);
            
        case 'dock'
            switch get(UD.Options.Dock,'userdata')
                case 0
                    %
                    % from docked to undocked
                    %
                    pos = get(mfig,'position');
                    pos2 = pos;
                    hshift = pos(3) - 180;
                    pos2(1) = pos(1) + hshift;
                    pos2(3) = 180;
                    pos(3) = hshift;
                    %
                    ofig = qp_uifigure('Plot Options','dock','Plot Options',pos2,'d3d_qp');
                    set(ofig,'resizefcn','d3d_qp optionsresize','resize','on')
                    %
                    set(UD.Options.Dock,'parent',ofig)
                    qp_tooltip(UD.Options.Dock,'Dock plot options')
                    %
                    set(UD.Options.Slider,'parent',ofig)
                    %
                    set(UOH,'parent',ofig)
                    %
                    A = allchild(ofig);
                    p = get(A,{'position'});
                    p = cat(1,p{:});
                    p(:,1) = p(:,1) - hshift;
                    p = num2cell(p,2);
                    set(A,{'position'},p)
                    %
                    set(UD.Options.Dock, ...
                        'CData',qp_icon('dock','nan'), ...
                        'UserData',1)
                    set(mfig,'position',pos)
                    set(ofig,'visible','on')
                    set(findall(mfig,'tag','dock','type','uimenu'),'visible','on')
                    %
                    UD.Options.ActPos(:,1) = UD.Options.ActPos(:,1) - hshift;
                    UD.Options.Pos(:,1) = UD.Options.Pos(:,1) - hshift;
                    setappdata(mfig,'QPHandles',UD)
                case 1
                    %
                    % from undocked to docked
                    %
                    set(findall(mfig,'tag','dock','type','uimenu'),'visible','off')
                    pos = get(mfig,'position');
                    hshift = pos(3);
                    pos(3) = pos(3) + 180;
                    %
                    ofig = get(UD.Options.Dock,'parent');
                    %
                    A = allchild(ofig);
                    p = get(A,{'position'});
                    p = cat(1,p{:});
                    p(:,1) = p(:,1) + hshift;
                    p = num2cell(p,2);
                    set(A,{'position'},p)
                    %
                    spos = get(UD.Options.Slider,'position');
                    spos(4) = pos(4)-15;
                    set(UD.Options.Slider,'parent',mfig,'position',spos)
                    %
                    dpos = get(UD.Options.Dock,'position');
                    dpos(2) = pos(4)-15;
                    set(UD.Options.Dock,'parent',mfig,'position',dpos)
                    qp_tooltip(UD.Options.Dock,'Undock plot options')
                    %
                    set(UOH,'parent',mfig)
                    delete(ofig)
                    %
                    set(UD.Options.Dock, ...
                        'CData',qp_icon('undock','nan'), ...
                        'UserData',0)
                    set(mfig,'position',pos)
                    %
                    UD.Options.ActPos(:,1) = UD.Options.ActPos(:,1) + hshift;
                    UD.Options.Pos(:,1) = UD.Options.Pos(:,1) + hshift;
                    %
                    update_option_positions(UD,pos(4)-30+1)
            end
            
        case {'climmin','climmax','1vecunit','vscale','thinfact','thindist','fontsize','linewidth'}
            switch cmd,
                case 'climmin'
                    pos=0;
                    int=0;
                case 'climmax'
                    pos=0;
                    int=0;
                case '1vecunit'
                    pos=1;
                    int=0;
                case 'vscale'
                    pos=1;
                    int=0;
                case 'thinfact'
                    pos=1;
                    int=1;
                case 'thindist'
                    pos=1;
                    int=0;
                case 'fontsize'
                    pos=1;
                    int=0;
                case 'linewidth'
                    pos=1;
                    int=0;
            end
            contr=findobj(UOH,'tag',[cmd '=?']);
            if isempty(cmdargs)
                contrv=str2num(get(contr,'string'));
            else
                contrv=cmdargs{1};
            end
            contrvdef=get(contr,'userdata');
            if isempty(contrv)
                contrv=contrvdef;
            elseif ~isequal(size(contrv),[1 1])
                contrv=contrv(1);
            elseif pos && (contrv<=0)
                contrv=contrvdef;
            elseif int && (contrv~=round(contrv) || ~isfinite(contrv))
                contrv=contrvdef;
            end
            set(contr,'string',sprintf('%g',contrv),'userdata',contrv)
            if logfile
                writelog(logfile,logtype,cmd,contrv);
            end
            
        case 'update_addtoplot'
            qv=findobj(mfig,'tag','quickview');
            atp= findobj(mfig,'tag','addtoplot');
            if ~isempty(UD.PlotMngr.CurrentAxes) && ishandle(UD.PlotMngr.CurrentAxes) && strcmp(get(qv,'enable'),'on')
                axestype=getappdata(UD.PlotMngr.CurrentAxes,'AxesType');
                %
                % Temporarily replace Lon-Lat by X-Y.
                %
                if ischar(axestype)
                    axestype=strrep(axestype,'Lon-Lat','X-Y');
                end
                %
                if ~ischar(axestype) || strcmp(axestype,UD.State.axestype)
                    %
                    % perfect match of axes types (including units in case of Val)
                    %
                    set(atp,'enable','on','foregroundcolor','k')
                else
                    %
                    % no exact match
                    %
                    if strcmp(strtok(axestype),strtok(UD.State.axestype)) && ...
                            (isempty(strfind(axestype,' ')) || ...
                            isempty(strfind(UD.State.axestype,' ')))
                        %
                        % if one of the two axes types lacks a unit specifier,
                        % and the axes types without unit specifier match, then
                        % still okay to combine plots.
                        %
                        set(atp,'enable','on','foregroundcolor','k')
                    else
                        set(atp,'enable','on','foregroundcolor','r')
                    end
                end
            else
                set(atp,'enable','off','foregroundcolor','k')
            end
            
        case 'close'
            if logfile
                try
                    writelog(logfile,logtype,cmd);
                    fclose(logfile);
                end
            end
            try
                Pos=get(mfig,'position');
                qp_settings('MainPositionUR',Pos(1:2)+Pos(3:4))
                if isstruct(UD) && isstruct(UD.PlotMngr)
                    Pos=get(UD.PlotMngr.Fig,'position');
                    qp_settings('PlotMngrPositionUL',Pos(1:2)+[0 Pos(4)])
                    Vis=get(UD.PlotMngr.Fig,'visible');
                    qp_settings('PlotMngrVisible',Vis)
                end
                qp_settings('<SAVE>')
            catch
            end
            if isstandalone
                close all force
            else
                if isstruct(UD)
                    if isstruct(UD.PlotMngr) && ishandle(UD.PlotMngr.Fig)
                        delete(UD.PlotMngr.Fig);
                    end
                    if isstruct(UD.FilOpt) && ishandle(UD.FilOpt.Fig)
                        delete(UD.FilOpt.Fig);
                    end
                    if isstruct(UD.GridView) && ishandle(UD.GridView.Fig)
                        delete(UD.GridView.Fig);
                    end
                    if isstruct(UD.ComLine) && ishandle(UD.ComLine.Fig)
                        delete(UD.ComLine.Fig);
                    end
                    if isstruct(UD.Options) && ishandle(UD.Options.Slider)
                        delete(get(UD.Options.Slider,'parent'));
                    end
                end
                if ishandle(mfig)
                    delete(mfig);
                end
            end
            qpversion=[];
            
        case {'about','version'}
            qp_showabout(qpversion,'quickplot');
            
        case {'aboutmatlab'}
            qp_showabout(qpversion,'matlab');
            
        case {'plotmngr','hideplotmngr'}
            currentstatus=get(UD.PlotMngr.Fig,'visible');
            if strcmp(cmd,'hideplotmngr'),
                currentstatus='on';
            end
            switch currentstatus
                case 'on'
                    setvis='off';
                    reverse='on';
                case 'off'
                    setvis='on';
                    reverse='off';
            end
            set(UD.PlotMngr.Fig,'visible',setvis)
            set(findobj(mfig,'label','Hide &Plot Manager'),'visible',setvis)
            set(findobj(mfig,'label','Show &Plot Manager'),'visible',reverse)
            set(UD.MainWin.TB_PM,'state',setvis)
            
        case {'comline','hidecomline'}
            currentstatus=get(UD.ComLine.Fig,'visible');
            if strcmp(cmd,'hidecomline'),
                currentstatus='on';
            end
            switch currentstatus
                case 'on'
                    setvis='off';
                    reverse='on';
                case 'off'
                    setvis='on';
                    reverse='off';
            end
            set(UD.ComLine.Fig,'visible',setvis)
            set(findobj(mfig,'label','Hide C&ommand Line'),'visible',setvis)
            set(findobj(mfig,'label','Show C&ommand Line'),'visible',reverse)
            set(UD.MainWin.TB_CL,'state',setvis)
            %
            % Commands not used in QuickPlot code, but useful to include for
            % execution from the command line...
            %
            if 0
                box
                grid
                camlight
                lighting phong;
                material dull;
                imagesc;
                xlim;
                ylim;
                zlim;
                qpsf;
                qpsa;
            end
            
        case 'showmessagewin'
            ui_message
            
        case 'evalhist'
            pos=get(gcbo,'position');
            set(UD.ComLine.Eval,'enable','on')
            set(UD.ComLine.EvalHistMenu,'position',pos(1:2)+pos(3:4)/2,'visible','on')
            
        case 'evalhistmenu'
            set(UD.ComLine.Eval,'string',get(gcbo,'userdata'),'enable','on')
            
        case 'evalcmd'
            evalcmd=get(UD.ComLine.Eval,'string');
            set(UD.ComLine.Eval,'enable','inactive')
            hvoff=findall(0,'handlevisibility','callback');
            set(hvoff,'handlevisibility','off')
            evalerr=0;
            if ~isempty(evalcmd)
                if evalcmd(1)=='>'
                    % MATLAB command
                    evalcmd=evalcmd(2:end);
                    ans='No value returned';
                    defaultans=ans;
                    try
                        eval(evalcmd);
                        if ~isequal(ans,defaultans)
                            ui_message('warning',var2str(ans))
                        end
                    catch
                        ui_message('error',{evalcmd,lasterr})
                        evalerr=1;
                    end
                    %
                    % take care of logging ...
                    %
                    if logfile && ~evalerr
                        writelog(logfile,-logtype,evalcmd);
                    end
                else
                    % QUICKPLOT command
                    %
                    % command itself will take care of logging ...
                    %
                    try
                        [cmd,cmdargs]=qp_cmdstr(evalcmd);
                        d3d_qp(cmd,cmdargs{:});
                    catch
                        errmsg=multiline(lasterr,'cell');
                        if isstandalone
                            i=1;
                        else
                            i=2;
                        end
                        ui_message('error',{evalcmd,errmsg{i:end}})
                        evalerr=1;
                    end
                end
            end
            set(hvoff,'handlevisibility','callback')
            if ~isempty(evalcmd) && ~evalerr
                evalcmd=get(UD.ComLine.Eval,'string');
                qp_update_evalhistmenu(UD.ComLine.EvalHistMenu,'add',evalcmd)
                set(UD.ComLine.Eval,'string','')
            end
            set(UD.ComLine.Eval,'enable','on')
            
        case 'gridview_selected'
            Rng=qp_gridview('getrange',UD.GridView.Fig);
            if isstruct(Rng) && isfield(Rng,'Type')
                switch Rng.Type
                    case 'line'
                        if isfinite(Rng.Range(1))
                            d3d_qp('allm*',0)
                            d3d_qp('editm*',Rng.Range(1))
                            d3d_qp('alln*',1)
                        else %isfinite(Rng.Range(2))
                            d3d_qp('allm*',1)
                            d3d_qp('alln*',0)
                            d3d_qp('editn*',Rng.Range(2))
                        end
                    case {'lineseg','range'}
                        if iscell(Rng.Range)
                            d3d_qp('allm*',0)
                            d3d_qp('editm*',Rng.Range{1})
                            if length(Rng.Range)>1
                                d3d_qp('alln*',0)
                                d3d_qp('editn*',Rng.Range{2})
                            end
                        else
                            d3d_qp('allm*',0)
                            r=[Rng.Range(1) Rng.Range(2)];
                            d3d_qp('editm*',min(r):max(r))
                            d3d_qp('alln*',0)
                            r=[Rng.Range(3) Rng.Range(4)];
                            d3d_qp('editn*',min(r):max(r))
                        end
                    case 'point'
                        d3d_qp('allm*',0)
                        d3d_qp('editm*',Rng.Range(1))
                        if length(Rng.Range)>1
                            d3d_qp('alln*',0)
                            d3d_qp('editn*',Rng.Range(2))
                        end
                    case 'pwline'
                        d3d_qp('editmn*',Rng.Range)
                    case 'genline'
                        d3d_qp('editxy*',Rng.Range)
                    case 'wholegrid'
                        d3d_qp('allm*',1)
                        d3d_qp('alln*',1)
                end
            end
            
        case 'gridview_update'
            if strcmp(get(UD.GridView.Fig,'visible'),'on')
                MW = UD.MainWin;
                switch getvalstr(MW.HSelType)
                    case 'M range and N range'
                        range={};
                        %----
                        if strcmp(get(MW.M,'enable'),'on')
                            allm=get(MW.AllM,'value');
                            if allm
                                maxm=get(MW.MaxM,'userdata');
                                range{1}=1:maxm;
                            else
                                mm=get(MW.EditM,'userdata');
                                range{1}=mm;
                            end
                        end
                        %----
                        if strcmp(get(MW.N,'enable'),'on')
                            alln=get(MW.AllN,'value');
                            if alln
                                maxn=get(MW.MaxN,'userdata');
                                range{2}=1:maxn;
                            else
                                nn=get(MW.EditN,'userdata');
                                range{2}=nn;
                            end
                        end
                        %----
                    case '(M,N) point/path'
                        range.Type='pwline';
                        range.Range = get(MW.EditMN,'userdata');
                    case '(X,Y) point/path'
                        range.Type='genline';
                        range.Range = get(MW.EditXY,'userdata');
                end
                qp_gridview('setrange',UD.GridView.Fig,range)
            end
            
        case {'showgridviewldb'}
            OpenFile=findobj(mfig,'tag','openfile','type','uipushtool');
            pn=get(OpenFile,'userdata');
            [FI,FileName,Tp,Otherargs]=qp_fmem('openldb',pn);
            GVAxes=findobj(UD.GridView.Fig,'type','axes');
            zoomupdate=0;
            switch Tp
                case {'Tekal','BNA File','ArcInfoUngenerate','ESRI-Shape'}
                    if strcmp(Tp,'Tekal') && ~isfield(FI,'combinelines')
                        ui_message('error','This Tekal file is not supported as supplier of landboundary data.')
                    end
                    LDB=qpread(FI,'line','griddata');
                    delete(findall(GVAxes,'tag','landboundary'))
                    ldbcol=qp_settings('gridviewlandboundarycolor')/255;
                    line(LDB.X,LDB.Y,'color',ldbcol,'parent',GVAxes,'hittest','off','tag','landboundary','clipping','off')
                    zoomupdate=1;
                case ''
                    delete(findall(GVAxes,'tag','landboundary'))
                    zoomupdate=1;
                otherwise
                    ui_message('error',sprintf('%s not supported as supplier of landboundary data.',Tp))
            end
            
            if zoomupdate
                xl=limits(GVAxes,'xlim'); xl=xl+[-1 1]*max(0.00001,abs(diff(xl)*0.01))/20;
                yl=limits(GVAxes,'ylim'); yl=yl+[-1 1]*max(0.00001,abs(diff(yl)*0.01))/20;
                if ~isfinite(xl)
                    xl=[0 1];
                    yl=[0 1];
                end
                set(GVAxes,'xlim',xl,'ylim',yl)
                delete(get(GVAxes,'zlabel')) % delete the old ZOOMAxesData applicationdata
                zoom(UD.GridView.Fig,'reset');
                zoom(UD.GridView.Fig,'on')
            end
            
        case {'gridview','hidegridview'}
            currentstatus=get(UD.GridView.Fig,'visible');
            if strcmp(cmd,'hidegridview'),
                currentstatus='on';
            end
            switch currentstatus
                case 'on'
                    setvis='off';
                    reverse='on';
                case 'off'
                    setvis='on';
                    reverse='off';
            end
            set(UD.GridView.Fig,'visible',setvis)
            set(findobj(mfig,'label','Hide &Grid View'),'visible',setvis)
            set(findobj(mfig,'label','Show &Grid View'),'visible',reverse)
            set(UD.MainWin.TB_GV,'state',setvis)
            if strcmp(setvis,'on')
                datafields=findobj(mfig,'tag','selectfield');
                Props=get(datafields,'userdata');
                fld=get(datafields,'value');
                Handle_SelectFile=findobj(mfig,'tag','selectfile');
                File=get(Handle_SelectFile,'userdata');
                NrInList=get(Handle_SelectFile,'value');
                Succes=~isempty(File);
                if ~Succes || isempty(Props) || ~isfield(Props,'DimFlag') || ~isempty(strmatch('---',Props(fld).Name))
                    qp_gridview('setgrid',UD.GridView.Fig,[],[])
                    set(UD.GridView.Fig,'userdata',[])
                elseif isfield(Props(fld),'UseGrid') && Props(fld).UseGrid>0
                    Info=File(NrInList);
                    %
                    Handle_Domain=findobj(mfig,'tag','selectdomain');
                    DomainNr=get(Handle_Domain,'value');
                    %
                    UseGrid=get(UD.GridView.Fig,'userdata');
                    i_grd=Props(fld).UseGrid;
                    UseGridNew={Info.Name,DomainNr,i_grd};
                    if ~isequal(UseGrid,UseGridNew)
                        set(UD.GridView.Fig,'name','Grid selection: updating grid ...')
                        [Chk,GRID]=qp_getdata(Info,DomainNr,Props(i_grd),'grid');
                        qp_gridview('setgrid',UD.GridView.Fig,GRID)
                        set(UD.GridView.Fig,'name','Grid selection')
                        set(UD.GridView.Fig,'userdata',UseGridNew)
                    end
                    d3d_qp('gridview_update')
                else
                    qp_gridview('setgrid',UD.GridView.Fig,[],[])
                    set(UD.GridView.Fig,'userdata',[])
                end
            end
            
        case 'stoprecord'
            stopped=0;
            if (logtype==1 && logfile==0) || logtype>2
                ButtonName='N/A';
                stopped=1;
            else
                rctype={'log','Matlab script'};
                rctype=rctype{logtype};
                ButtonName=questdlg( ...
                    sprintf('This will stop writing the %s file',rctype), ...
                    rctype, ...
                    'OK','Cancel','Cancel');
            end
            if strcmp(ButtonName,'OK')
                fclose(logfile);
                stopped=1;
            end
            if stopped
                set(findobj(mfig,'tag','stoprecord'),'enable','off')
                set(UD.MainWin.StartRec,'enable','on')
                set(UD.MainWin.RecTB,'enable','on')
                logfile=0;
            end
            
        case {'logfile','scriptfile','logfiletomw','scriptfiletomw','recordtomw','scriptfiletomcw'}
            switch cmd
                case 'logfile'
                    logtype=1;
                    ftype='*.qplog';
                case 'scriptfile'
                    logtype=2;
                    ftype='*.m';
                case {'recordtomw','logfiletomw'}
                    logtype=3; % Quickplot Log file
                case 'scriptfiletomw'
                    logtype=4; % MATLAB statements
                case 'scriptfiletomcw'
                    logtype=6; % MATLAB statements
            end
            if logtype>2
                logfile=1;
                set(findobj(mfig,'tag','stoprecord'),'enable','on')
                set(UD.MainWin.StartRec,'enable','off')
                set(UD.MainWin.RecTB,'enable','off')
                if logtype==3 || logtype==4
                    ui_message
                end
            else
                logfile=0;
                [fn,pn]=uiputfile(ftype);
                if ischar(fn)
                    [lp,ln,le]=fileparts(fn);
                    if isempty(le)
                        fn=[fn ftype(2:end)];
                    end
                    try
                        logfile=fopen([pn fn],'w');
                    catch
                        ui_message('error',{'Catch in d3d_qp\logfile',lasterr})
                    end
                    set(findobj(mfig,'tag','stoprecord'),'enable','on')
                    set(UD.MainWin.StartRec,'enable','off')
                    set(UD.MainWin.RecTB,'enable','off')
                end
            end
            
        case 'newfigure'
            [h,figops,createops]=qp_createfig(cmdargs{:});
            if ~isempty(h)
                UDplot=get(h,'userdata');
                UDplot.ProgID='QuickPlot';
                set(h,'userdata',UDplot);
                if ~isempty(h)
                    set(UD.PlotMngr.FigList,'value',1,'string',listnames(h,'showType','no','showHandle','no','showTag','no'),'userdata',h);
                    d3d_qp refreshfigs
                end
                if logfile
                    writelog(logfile,logtype,cmd,createops{:});
                end
            end
            
        case 'newaxes'
            FigIDs=get(UD.PlotMngr.FigList,'userdata');
            if isempty(FigIDs)
                set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                    'enable','off','backgroundcolor',Inactive);
                set(UD.PlotMngr.DelAx,'enable','off');
                UD.PlotMngr.CurrentAxes=[];
                setappdata(mfig,'QPHandles',UD)
                d3d_qp refreshitems
            else
                FigVal=get(UD.PlotMngr.FigList,'value');
                Fig=FigIDs(FigVal);
                if ~ishandle(Fig)
                    d3d_qp refreshfigs
                else
                    h=qp_createaxes(Fig);
                    if ~isempty(h)
                        set(UD.PlotMngr.AxList,'value',1,'string',listnames(h),'userdata',h);
                        d3d_qp refreshaxs
                    end
                end
            end
            
        case 'openfigure'
            figuredir=qp_settings('figuredir');
            if ~isempty(cmdargs)
                [p,f,extension] = fileparts(cmdargs{1});
                f = [f,extension];
            else
                pf = fullfile(figuredir,'*.fig');
                [f,p]=uigetfile(pf,'Open figure ...');
            end
            if ischar(f)
                figuredir=p;
                qp_settings('figuredir',figuredir)
                %
                pf = fullfile(p,f);
                h=hgload(pf);
                set(h,'menubar','none','closerequestfcn','d3d_qp closefigure')
                qp_figurebars(h)
                %set(cbar,'deletefcn','qp_colorbar delete')
                hName = listnames(h,'showtype','no','showhandle','no','showtag','no');
                set(UD.PlotMngr.FigList,'value',1,'string',hName,'userdata',h);
                d3d_qp refreshfigs
                if logfile
                    writelog(logfile,logtype,cmd,pf);
                end
            end
            
        case 'refreshfigs'
            if ~isempty(cmdargs)
                Fg=cmdargs{1};
                FgName = listnames(Fg,'showtype','no','showhandle','no','showtag','no');
                set(UD.PlotMngr.FigList,'value',1,'string',FgName,'userdata',Fg);
            end
            
            Figs=get_nondialogs;
            if isempty(Figs)
                set(UD.PlotMngr.FigList,'string',{''},'userdata',[],'value',1, ...
                    'enable','off','backgroundcolor',Inactive);
                set(UD.PlotMngr.SavFig,'enable','off');
                set(UD.PlotMngr.ClsFig,'enable','off');
                set(UD.PlotMngr.FigAll,'enable','off');
                set(UD.PlotMngr.NewAx,'enable','off');
            else
                fignames=listnames(Figs,'showtype','no','showhandle','no','showtag','no');
                [fignames,Order]=sort(fignames);
                Figs=Figs(Order);
                FigNms=get(UD.PlotMngr.FigList,'string');
                FigHnd=get(UD.PlotMngr.FigList,'userdata');
                FigVal=get(UD.PlotMngr.FigList,'value');
                if FigVal<=length(FigHnd) && ismember(FigHnd(FigVal),Figs)
                    i=find(Figs==FigHnd(FigVal));
                elseif FigVal<=length(FigNms)
                    if iscell(FigNms)
                        FigNm=FigNms{FigVal};
                    else
                        FigNm=deblank(FigNms(FigVal,:));
                    end
                    i=strmatch(FigNm,fignames,'exact');
                    if isempty(i)
                        i=1;
                    end
                else
                    i=1;
                end
                set(UD.PlotMngr.FigAll,'enable','on');
                enable = 'on';
                backgroundcolor = Active;
                if get(UD.PlotMngr.FigAll,'value')
                   enable = 'off';
                   backgroundcolor = Inactive;
                end
                set(UD.PlotMngr.FigList,'string',fignames,'userdata',Figs,'value',i, ...
                    'enable',enable,'backgroundcolor',backgroundcolor);
                set(UD.PlotMngr.SavFig,'enable','on');
                set(UD.PlotMngr.ClsFig,'enable',enable);
                set(UD.PlotMngr.NewAx,'enable','on');
            end
            d3d_qp refreshaxs
            d3d_qp update_addtoplot
            
        case 'allfigures'
            if ~isempty(cmdargs)
                allFigs = cmdargs{1};
                set(UD.PlotMngr.FigAll,'value',allFigs)
            else
                allFigs = get(UD.PlotMngr.FigAll,'value');
            end
            if allFigs
                figlistenable='off';
                figlistcolour=Inactive;
            else
                figlistenable='on';
                figlistcolour=Active;
            end
            set(UD.PlotMngr.FigList, ...
                'enable',figlistenable,'backgroundcolor',figlistcolour);
            set(UD.PlotMngr.ClsFig,'enable',figlistenable);
            d3d_qp refreshaxs
            
        case 'refreshaxs'
            FigIDs=get(UD.PlotMngr.FigList,'userdata');
            if isempty(FigIDs)
                set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                    'enable','off','backgroundcolor',Inactive);
                set(UD.PlotMngr.DelAx,'enable','off');
                set(UD.PlotMngr.AxAll,'enable','off');
                UD.PlotMngr.CurrentAxes=[];
                setappdata(mfig,'QPHandles',UD)
            else
                FigVal=get(UD.PlotMngr.FigList,'value');
                allfigs=get(UD.PlotMngr.FigAll,'value');
                if allfigs
                    Fig=FigIDs;
                else
                    Fig=FigIDs(FigVal);
                end
                if any(~ishandle(Fig))
                    d3d_qp refreshfigs
                else
                    Axs=findall(Fig,'type','axes');
                    for i=length(Axs):-1:1
                        if isappdata(Axs(i),'NonDataObject')
                            Axs(i)=[];
                        end
                    end
                    if isempty(Axs)
                        set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                            'enable','off','backgroundcolor',Inactive);
                        set(UD.PlotMngr.DelAx,'enable','off');
                        UD.PlotMngr.CurrentAxes=[];
                        setappdata(mfig,'QPHandles',UD)
                    else
                        axnames=listnames(Axs);
                        AxVal=get(UD.PlotMngr.AxList,'value');
                        
                        AxH=get(UD.PlotMngr.AxList,'userdata');
                        if AxVal>length(AxH)
                            i=1;
                        else
                            i=find(AxH(AxVal)==Axs);
                            if isempty(i)
                                i=1;
                            end
                        end
                        axallenabled='on';
                        if allfigs
                            axallenabled='off';
                        end
                        set(UD.PlotMngr.AxAll,'enable',axallenabled);
                        axlistenable='on';
                        axlistcolour=Active;
                        if get(UD.PlotMngr.AxAll,'value') || allfigs
                            axlistenable='off';
                            axlistcolour=Inactive;
                        end
                        set(UD.PlotMngr.AxList,'string',axnames,'userdata',Axs,'value',i, ...
                            'enable',axlistenable,'backgroundcolor',axlistcolour);
                        set(UD.PlotMngr.DelAx,'enable',axlistenable);
                        UD.PlotMngr.CurrentAxes=Axs(i);
                        setappdata(mfig,'QPHandles',UD)
                    end
                end
            end
            d3d_qp refreshitems
            
        case 'allaxes'
            if ~isempty(cmdargs)
                allAxes = cmdargs{1};
                set(UD.PlotMngr.AxAll,'value',allAxes)
            else
                allAxes = get(UD.PlotMngr.AxAll,'value');
            end
            if allAxes
                axlistenable='off';
                axlistcolour=Inactive;
            else
                axlistenable='on';
                axlistcolour=Active;
            end
            set(UD.PlotMngr.AxList, ...
                'enable',axlistenable,'backgroundcolor',axlistcolour);
            set(UD.PlotMngr.DelAx,'enable',axlistenable);
            d3d_qp refreshitems
            
        case 'refreshitems'
            AxIDs=get(UD.PlotMngr.AxList,'userdata');
            if isempty(AxIDs)
                set(UD.PlotMngr.ItList,'string',{''},'userdata',[],'value',1, ...
                    'enable','off','backgroundcolor',Inactive);
                set(UD.PlotMngr.DelIt,'enable','off');
                set(UD.PlotMngr.ItInfo,'enable','off');
                set(UD.PlotMngr.ItLink,'enable','off');
            else
                AxVal=get(UD.PlotMngr.AxList,'value');
                if get(UD.PlotMngr.AxAll,'value') || get(UD.PlotMngr.FigAll,'value')
                    Ax=AxIDs;
                else
                    Ax=AxIDs(AxVal);
                end
                if ~ishandle(Ax)
                    d3d_qp refreshaxs
                else
                    Items=allchild(Ax);
                    if iscell(Items)
                        Items(:,2)={0};
                        Items(end,2)={[]};
                        Items=Items';
                        Items=cat(1,Items{:});
                    end
                    Tags=get(Items,'tag');
                    for t=find(Items==0)'
                        Tags(t)={sprintf('QPPlotTag---%i',t)};
                    end
                    UserDatas=get(Items,'userdata');
                    UserDatas(Items==0)={'---'};
                    %---
                    TUDvalid=~cellfun('isempty',Tags) & ~cellfun('isempty',UserDatas);
                    Items=Items(TUDvalid);
                    Tags=Tags(TUDvalid);
                    UserDatas=UserDatas(TUDvalid);
                    %---
                    QPTag=strmatch('QPPlotTag',Tags);
                    Items=Items(QPTag);
                    Tags=Tags(QPTag);
                    UserDatas=UserDatas(QPTag);
                    %---
                    [Tags,I]=unique(Tags);
                    [I,Isort]=sort(I);
                    Tags=Tags(Isort);
                    Items=Items(I);
                    UserDatas=UserDatas(I);
                    %---
                    while ~isempty(Items) && Items(end)==0
                        Items(end)=[];
                        UserDatas(end)=[];
                        Tags(end)=[];
                    end
                    %---
                    while ~isempty(Items) && Items(1)==0
                        Items(1)=[];
                        UserDatas(1)=[];
                        Tags(1)=[];
                    end
                    %---
                    separator='------';
                    if isempty(Items)
                        set(UD.PlotMngr.ItList,'string',{''},'userdata',[],'value',1, ...
                            'enable','off','backgroundcolor',Inactive);
                        set(UD.PlotMngr.DelIt,'enable','off');
                        set(UD.PlotMngr.ItInfo,'enable','off');
                        set(UD.PlotMngr.ItLink,'enable','off');
                    else
                        OldTags=get(UD.PlotMngr.ItList,'userdata');
                        if isempty(OldTags)
                            i=1;
                        else
                            ItVal=get(UD.PlotMngr.ItList,'value');
                            SelTag=OldTags{1}(ItVal);
                            i=strmatch(SelTag,Tags,'exact');
                            if isempty(i)
                                i=1;
                            end
                        end
                        prevseparator=0;
                        it=length(Items);
                        if it>0
                            prevseparator=1;
                        end
                        while it>=1
                            %
                            % Backward compatible with cell version of PlotState ...
                            %
                            if isequal(UserDatas{it},'---')
                                Nms{it}=separator;
                                if prevseparator
                                    Nms(it)=[];
                                    UserDatas(it)=[];
                                    Items(it)=[];
                                    Tags(it)=[];
                                end
                                prevseparator=1;
                            elseif iscell(UserDatas{it}.PlotState)
                                Nms{it}=UserDatas{it}.PlotState{2}.Name;
                                prevseparator=0;
                            else
                                Nms{it}=UserDatas{it}.PlotState.Props.Name;
                                prevseparator=0;
                            end
                            it=it-1;
                        end
                        if prevseparator
                            Nms(1)=[];
                            UserDatas(1)=[];
                            Items(1)=[];
                            Tags(1)=[];
                        end
                        for it=1:length(Items)
                            it_same_name=strmatch(Nms{it},Nms,'exact');
                            extend=1;
                            while (length(it_same_name)>1) && ~strcmp(Nms{it},separator)
                                for itloc=it_same_name'
                                    switch extend
                                        case 1
                                            extrastr{itloc}=abbrevfn(UserDatas{itloc}.PlotState.FI.Name);
                                        case 2
                                            stat=UserDatas{itloc}.PlotState.Selected{ST_};
                                            if ~isempty(stat)
                                                %stats=qpread(UserDatas{itloc}.PlotState.FI,UserDatas{itloc}.PlotState.Props,'stations');
                                                stats=UserDatas{itloc}.PlotState.Stations;
                                                if length(stat)>1
                                                    extrastr{itloc}=['ST=' vec2str(stat,'nobrackets')];
                                                elseif stat==0
                                                    extrastr{itloc}='all';
                                                elseif iscell(stats)
                                                    extrastr{itloc}=stats{stat};
                                                else
                                                    extrastr{itloc}=deblank(stats(stat,:));
                                                end
                                            end
                                        case 3
                                            if ~isempty(UserDatas{itloc}.PlotState.SubField)
                                                subflds=qpread(UserDatas{itloc}.PlotState.FI,UserDatas{itloc}.PlotState.Props,'subfields');
                                                extrastr{itloc}=subflds{UserDatas{itloc}.PlotState.SubField{1}};
                                            else
                                                extrastr{itloc}='';
                                            end
                                        case 4
                                            m=UserDatas{itloc}.PlotState.Selected{M_};
                                            if iscell(m)
                                                extrastr{itloc}=[m{1} ' line'];
                                            elseif isequal(m,0)
                                                extrastr{itloc}='All M';
                                            elseif ~isempty(m)
                                                extrastr{itloc}=['M=' vec2str(m,'nobrackets')];
                                            end
                                        case 5
                                            n=UserDatas{itloc}.PlotState.Selected{N_};
                                            if isequal(n,0)
                                                extrastr{itloc}='All N';
                                            elseif ~isempty(n)
                                                extrastr{itloc}=['N=' vec2str(n,'nobrackets')];
                                            end
                                        case 6
                                            k=UserDatas{itloc}.PlotState.Selected{K_};
                                            if isequal(k,0)
                                                extrastr{itloc}='All K';
                                            elseif ~isempty(k)
                                                extrastr{itloc}=['K=' vec2str(k,'nobrackets')];
                                            end
                                        case 8
                                            t=UserDatas{itloc}.PlotState.Selected{T_};
                                            if isequal(t,0)
                                                extrastr{itloc}='All TS';
                                            elseif ~isempty(t)
                                                extrastr{itloc}=['TS=' vec2str(t,'nobrackets')];
                                            end
                                        case 7
                                            extrastr{itloc}=UserDatas{itloc}.PlotState.Ops.presentationtype;
                                        otherwise
                                            extrastr={};
                                            break
                                    end
                                end
                                if isempty(extrastr)
                                    break
                                end
                                it_extra_same=strmatch(extrastr{it},extrastr(it_same_name),'exact');
                                if length(it_extra_same)<length(it_same_name)
                                    for itloc=it_same_name'
                                        Nms{itloc}=cat(2,Nms{itloc},' - ',extrastr{itloc});
                                    end
                                    it_same_name=it_same_name(it_extra_same);
                                end
                                extend=extend+1;
                            end
                        end
                        %
                        % try not to select a separator
                        %
                        val=1;
                        while val<length(Nms) && strcmp(Nms{val},separator)
                            val=val+1;
                        end
                        %
                        % if there are only separators, select none
                        %
                        if strcmp(Nms{val},separator)
                            val=[];
                        end
                        set(UD.PlotMngr.ItList,'string',Nms,'userdata',{Tags Items},'value',val, ...
                            'enable','on','backgroundcolor',Active);
                        %
                        % buttons should not be enabled if a separator is selected
                        %
                        enable='on';
                        if isempty(val)
                            enable='off';
                        end
                        set(UD.PlotMngr.DelIt,'enable',enable);
                        set(UD.PlotMngr.ItInfo,'enable',enable);
                        set(UD.PlotMngr.ItLink,'enable',enable);
                    end
                end
            end
            
        case 'itemlist'
            AxIDs=get(UD.PlotMngr.AxList,'userdata');
            AxVal=get(UD.PlotMngr.AxList,'value');
            if get(UD.PlotMngr.FigAll,'value')
                Ax=AxIDs;
            elseif get(UD.PlotMngr.AxAll,'value')
                Ax=AxIDs;
            else
                Ax=AxIDs(AxVal);
            end
            if ~ishandle(Ax)
                d3d_qp refreshaxs
            else
                ItInfo=get(UD.PlotMngr.ItList,'userdata');
                ItVal=get(UD.PlotMngr.ItList,'value');
                ItTags=ItInfo{1};
                ItIDs=ItInfo{2};
                OK=1;
                ItVal(ItIDs(ItVal)==0)=[];
                for itVal=ItVal
                    ItTag=ItTags{itVal};
                    hIt=findall(Ax,'tag',ItTag);
                    if isempty(hIt)
                        d3d_qp refreshitems
                        OK=0;
                        break
                    end
                end
                if OK
                    set(UD.PlotMngr.ItList,'value',ItVal);
                    if length(ItVal)==1
                        UserDatas=get(hIt,'userdata');
                        if iscell(UserDatas)
                            UserDatas=UserDatas(~cellfun('isempty',UserDatas));
                            UserDatas=UserDatas{1};
                        end
                        set(UD.PlotMngr.DelIt,'enable','on');
                        set(UD.PlotMngr.ItLink,'enable','on');
                        set(UD.PlotMngr.ItInfo,'enable','on');
                    else
                        if length(ItVal)==0
                            set(UD.PlotMngr.DelIt,'enable','off');
                            set(UD.PlotMngr.ItLink,'enable','off');
                        end
                        set(UD.PlotMngr.ItInfo,'enable','off');
                    end
                end
            end
            
        case 'iteminfo'
            AxIDs=get(UD.PlotMngr.AxList,'userdata');
            AxVal=get(UD.PlotMngr.AxList,'value');
            Ax=AxIDs(AxVal);
            if ~ishandle(Ax)
                d3d_qp refreshaxs
            else
                pfig=get(Ax,'parent');
                ItIDs=get(UD.PlotMngr.ItList,'userdata');
                ItVal=get(UD.PlotMngr.ItList,'value');
                %ItTags=get(UD.PlotMngr.ItList,'string');
                ItTags=ItIDs{1};
                ItTag=ItTags{ItVal};
                
                hIt=findall(pfig,'tag',ItTag);
                UserDatas=get(hIt,'userdata');
                if iscell(UserDatas)
                    UserDatas=UserDatas(~cellfun('isempty',UserDatas));
                    UserDatas=UserDatas{1};
                end
                if isstruct(UserDatas)
                    if isfield(UserDatas,'XInfo') && ~isempty(UserDatas.XInfo)
                        locStruct.DataInfo=UserDatas.XInfo;
                    end
                    %
                    % Backward compatible with cell version of PlotState ...
                    %
                    if iscell(UserDatas.PlotState)
                        nm=UserDatas.PlotState{2}.Name;
                        locStruct.PlotInfo=UserDatas.PlotState{end};
                    else
                        nm=UserDatas.PlotState.Props.Name;
                        locStruct.PlotInfo=UserDatas.PlotState.Ops;
                    end
                    ui_inspectstruct(locStruct,nm);
                end
                
            end
            
        case 'deleteaxes'
            AxIDs=get(UD.PlotMngr.AxList,'userdata');
            AxVal=get(UD.PlotMngr.AxList,'value');
            Ax=AxIDs(AxVal);
            if ishandle(Ax)
                pfig=get(Ax,'parent');
                Items=allchild(Ax);
                Tags=get(Items,'tag');
                UserDatas=get(Items,'userdata');
                TUDvalid=~cellfun('isempty',Tags) & ~cellfun('isempty',UserDatas);
                Tags=Tags(TUDvalid);
                QPTag=strmatch('QPPlotTag',Tags);
                Tags=Tags(QPTag);
                Tags=unique(Tags);
                for i=1:length(Tags)
                    ItTag=Tags{i};
                    hAnIt=findall(pfig,'userdata',ItTag);
                    if ~isempty(hAnIt)
                        delete(hAnIt)
                    end
                end
                delete(Ax)
            end
            d3d_qp refreshaxs
            
        case 'deleteitems'
            AxIDs=get(UD.PlotMngr.AxList,'userdata');
            AxVal=get(UD.PlotMngr.AxList,'value');
            if get(UD.PlotMngr.FigAll,'value') || get(UD.PlotMngr.AxAll,'value')
                Ax=AxIDs;
            else
                Ax=AxIDs(AxVal);
            end
            if ~ishandle(Ax)
                d3d_qp refreshaxs
            else
                pfig=get(Ax,'parent');
                if iscell(pfig)
                    pfig=unique(cat(1,pfig{:}));
                end
                ItIDs=get(UD.PlotMngr.ItList,'userdata');
                ItVal=get(UD.PlotMngr.ItList,'value');
                ItTags=ItIDs{1};
                for itVal=ItVal
                    ItTag=ItTags{itVal};
                    hIt=findall(pfig,'tag',ItTag); % the object itself
                    hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                    delete(hIt)
                    if ~isempty(hAnIt)
                        uicm=get(hAnIt(1),'parent');
                        delete(hAnIt)
                        ItFig=get(uicm,'parent');
                        animsld=findobj(ItFig,'tag','animslid');
                        if isempty(get(uicm,'children'))
                            animpush=findobj(ItFig,'tag','animpush');
                            set(animpush,'enable','off')
                        end
                        UD=get(animsld,'userdata');
                        iobj=1;
                        while iobj<=length(UD)
                            if strcmp(UD(iobj).Tag,ItTag)
                                UD(iobj)=[];
                            end
                            iobj=iobj+1;
                        end
                        animsldEnab='on';
                        if isempty(UD)
                            animsldEnab='off';
                        end
                        set(animsld,'enable',animsldEnab,'userdata',UD)
                    end
                end
                d3d_qp refreshitems
            end
            
        case 'linkitems'
            AxIDs=get(UD.PlotMngr.AxList,'userdata');
            AxVal=get(UD.PlotMngr.AxList,'value');
            if get(UD.PlotMngr.FigAll,'value') || get(UD.PlotMngr.AxAll,'value')
                Ax=AxIDs;
            else
                Ax=AxIDs(AxVal);
            end
            if any(~ishandle(Ax))
                d3d_qp refreshaxs
            else
                pfig=get(Ax,'parent');
                if iscell(pfig)
                    pfig=unique(cat(1,pfig{:}));
                end
                uicm=findall(pfig,'type','uicontextmenu');
                checked=findall(uicm,'checked','on');
                ItIDs=get(UD.PlotMngr.ItList,'userdata');
                ItVal=get(UD.PlotMngr.ItList,'value');
                ItTags=ItIDs{1};
                CanAnim={};
                for itVal=ItVal(1)
                    ItTag=ItTags{itVal};
                    %hIt=findall(Ax,'tag',ItTag); % the object itself
                    hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                    hCanAnim=get(hAnIt,'children');
                    for h=hCanAnim'
                        NRs=get(h,'userdata'); nsteps=NRs(2);
                        CanAnim{end+1,1}=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                    end
                end
                for itVal=ItVal
                    ItTag=ItTags{itVal};
                    %hIt=findall(Ax,'tag',ItTag); % the object itself
                    hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                    hCanAnim=get(hAnIt,'children');
                    CanAnimL={};
                    for h=hCanAnim'
                        NRs=get(h,'userdata'); nsteps=NRs(2);
                        CanAnimL{end+1,1}=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                    end
                    CanAnim=intersect(CanAnim,CanAnimL);
                    
                end
                if isempty(CanAnim)
                    if length(ItVal)>1
                        ui_message('error','Selected items do not have any animation field in common.')
                    else
                        ui_message('error','This item has no animation field.')
                    end
                    return
                end
                if length(CanAnim)>1
                    if ~isempty(cmdargs)
                        i = ustrcmpi(cmdargs{1},CanAnim);
                    else
                        i = -1;
                    end
                    if i>0
                        AnimateThis=CanAnim{i};
                    else
                        AnimateThis=ui_type(CanAnim);
                    end
                else
                    AnimateThis=CanAnim{1};
                end
                if isempty(AnimateThis)
                    return
                end
                set(checked,'checked','off')
                AnimObj=[];
                for itVal=ItVal
                    ItTag=ItTags{itVal};
                    %hIt=findall(Ax,'tag',ItTag); % the object itself
                    hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                    hCanAnim=get(hAnIt,'children');
                    for h=hCanAnim'
                        NRs=get(h,'userdata'); nsteps=NRs(2);
                        AniStr=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                        if strcmp(AniStr,AnimateThis)
                            set(h,'checked','on')
                            AnimObj(end+1).Fld=NRs(1);
                            NSteps=NRs(2);
                            AnimObj(end).Tag=ItTag;
                        end
                    end
                end
                animsld=findobj(pfig,'tag','animslid');
                hIt=findall(pfig,'tag',ItTags{ItVal(1)}); % get the first object
                hItUD=get(hIt,'userdata');
                if iscell(hItUD) % object consisting of multiple HG objects
                    mainIt=find(~cellfun('isempty',hItUD));
                    hItUD=hItUD{mainIt(1)};
                end
                %
                % Backward compatible with cell version of PlotState ...
                %
                if iscell(hItUD.PlotState)
                    hItUD.PlotState=plotstatestruct(hItUD.PlotState);
                end
                t_=AnimObj(1).Fld;
                if t_==0
                    t=hItUD.PlotState.SubField{1};
                else
                    t=hItUD.PlotState.Selected{t_};
                end
                AnimMax=NSteps;
                sstep=[min(1/(AnimMax-1),.1) min(10/(AnimMax-1),.9)];
                set(animsld,'userdata',AnimObj,'value',1,'sliderstep',sstep,'Max',AnimMax,'enable','on','value',t)
                Str=sprintf('%i',t);
                if t_==0
                    [Chk,sflds] = qp_getdata(hItUD.PlotState.FI, ...
                        hItUD.PlotState.Domain, ...
                        hItUD.PlotState.Props,'subfields',t);
                    if Chk
                        Str=sflds{1};
                    end
                end
                set(animsld,'tooltip',[DimStr{t_+1},Str]);
                qck_anim('slider',pfig(1))
            end
            
        case 'selectedfigure'
            FigureHandles=get(UD.PlotMngr.FigList,'userdata');
            if get(UD.PlotMngr.FigAll,'value')
                iFg=1:length(FigureHandles);
            else
                iFg=get(UD.PlotMngr.FigList,'value');
            end
            if isempty(FigureHandles)
                outdata = [];
            else
                outdata = FigureHandles(iFg);
            end
            
        case 'selectfigure'
            if ~isempty(cmdargs)
                h = cmdargs{1};
                if any(ishandle(h))
                    %
                    h(~ishandle(h))=[];
                    for i = length(h):-1:1
                        while ~isequal(get(h(i),'type'),'figure') && ~isequal(get(h(i),'type'),'root')
                            h(i) = get(h(i),'parent');
                        end
                        if isequal(get(h(i),'type'),'root')
                            h(i) = [];
                        end
                    end
                    h = unique(h);
                    %
                    FigureHandles=get(UD.PlotMngr.FigList,'userdata');
                    iFg=find(ismember(FigureHandles,h));
                    if length(iFg)<length(h)
                        %
                        % If a handle not found in list of figures, try
                        % once refreshing the list of figures.
                        %
                        d3d_qp refreshfigs
                        FigureHandles=get(UD.PlotMngr.FigList,'userdata');
                        iFg=find(ismember(FigureHandles,h));
                    end
                    %
                    % If still not found, continue without selecting the requested
                    % figure.
                    %
                    if ~isempty(iFg)
                        d3d_qp('allfigures',length(iFg)>1)
                        if length(iFg)==1
                            set(UD.PlotMngr.FigList,'value',iFg)
                        end
                    end
                    %
                end
            end
            d3d_qp refreshaxs
            
        case 'selectedaxes'
            AxesHandles=get(UD.PlotMngr.AxList,'userdata');
            if get(UD.PlotMngr.AxAll,'value') || get(UD.PlotMngr.FigAll,'value')
                iAx=1:length(AxesHandles);
            else
                iAx=get(UD.PlotMngr.AxList,'value');
            end
            if isempty(AxesHandles)
                outdata = [];
            else
                outdata = AxesHandles(iAx);
            end
            
        case 'selectaxes'
            if ~isempty(cmdargs)
                h = cmdargs{1};
                if ischar(h)
                    %
                    AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                    Tags = get(AxesHandles,'tag');
                    iAx = ustrcmpi(h,Tags);
                    if iAx<0
                        %
                        % If tag not found in list of axes, try once refreshing the
                        % list of axes.
                        %
                        d3d_qp refreshaxs
                        AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                        Tags = get(AxesHandles,'tag');
                        iAx = ustrcmpi(h,Tags);
                    end
                    %
                    % If still not found, continue without selecting the requested
                    % axes.
                    %
                    if iAx>0
                        set(UD.PlotMngr.AxList,'value',iAx)
                    end
                    %
                elseif any(ishandle(h))
                    %
                    h(~ishandle(h))=[];
                    for i = length(h):-1:1
                        while ~isequal(get(h(i),'type'),'axes') && ~isequal(get(h(i),'type'),'figure') && ~isequal(get(h(i),'type'),'root')
                            h(i) = get(h(i),'parent');
                        end
                        if isequal(get(h(i),'type'),'figure') || isequal(get(h(i),'type'),'root')
                            h(i) = [];
                        end
                    end
                    h = unique(h);
                    %
                    hFig = get(h,'parent');
                    if iscell(hFig)
                        hFig = cat(1,hFig{:});
                    end
                    d3d_qp('selectfigure',hFig);
                    %
                    AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                    iAx=find(ismember(AxesHandles,h));
                    if ~isempty(iAx)
                        d3d_qp('allaxes',length(iAx)>1)
                        if length(iAx)==1
                            set(UD.PlotMngr.AxList,'value',iAx)
                        end
                    end
                    %
                end
            end
            d3d_qp refreshaxs
            d3d_qp update_addtoplot
            
        case 'selecteditem'
            ItemNames=get(UD.PlotMngr.ItList,'string');
            ItemHandles=get(UD.PlotMngr.ItList,'userdata');
            iItem = get(UD.PlotMngr.ItList,'value');
            nItems = max(1,length(iItem));
            outdata(nItems).Name='dummy';
            outdata(nItems).Tag='dummy';
            if isempty(iItem)
                outdata(1)=[];
            else
                for i = nItems:-1:1
                    outdata(i).Name = ItemNames{iItem(i)}; 
                    outdata(i).Tag = ItemHandles{1}{iItem(i)};
                end
            end
            
        case 'selectitem'
            if ~isempty(cmdargs)
                h = cmdargs{1};
                if isstruct(h)
                    if ~isfield(h,'Tag')
                        error('Invalid structure provided select item call: missing Tag field!')
                    end
                    Tags = {h(:).Tag};
                elseif isnumeric(h)
                    Tags = get(h,'tag');
                    if isempty(Tags)
                        Tags = {};
                    elseif ischar(Tags)
                        Tags = {Tags};
                    end
                end
                %
                allObjs = findall(0);
                allTags = get(allObjs,'tag');
                %
                allObjs = allObjs(ismember(allTags,Tags));
                allAxs = get(allObjs,'parent');
                if iscell(allAxs)
                    allAxs = cat(1,allAxs{:});
                end
                %
                if ~isempty(allAxs)
                    d3d_qp('selectaxes',allAxs)
                    %
                    ItemHandles = get(UD.PlotMngr.ItList,'userdata');
                    iIt = find(ismember(ItemHandles{1},Tags));
                    set(UD.PlotMngr.ItList,'value',iIt)
                end
            end
            d3d_qp update_addtoplot
            
        case 'figcolor'
            c=uisetcolor(get(gcbf,'color'));
            if isequal(size(c),[1 3])
                set(gcbf,'color',c);
            end
            
        case 'zoomdown'
            zoom(gcbf,'down');
            ax=get(gcbf,'currentaxes');
            if ~isempty(ax)
                axestype=getappdata(ax,'AxesType');
                if ischar(axestype)
                    basicaxestype=strtok(axestype);
                    switch basicaxestype
                        case {'Time-Val','Time-Z','Time-<blocking>'}
                            set(ax,'xticklabelmode','auto','xtickmode','auto');
                            tick(ax,'x','autodate');
                        case {'LimitingFactorsAxes','LimitingFactorsAxes2'}
                            if isequal(basicaxestype,'LimitingFactorsAxes2')
                                ax2=ax;
                                ax=getappdata(ax2,'LimitingFactorsAxes');
                                set(ax,'xlim',get(ax2,'xlim'))
                            else
                                ax2 = getappdata(ax,'LimitingFactorsAxes');
                            end
                            set(ax,'xticklabelmode','auto','xtickmode','auto');
                            tick(ax,'x','autodate');
                            set(ax2,'xlim',get(ax,'xlim'), ...
                                'ylim',getappdata(ax2,'YLim'), ...
                                'xtick',get(ax,'xtick'), ...
                                'xticklabel',get(ax,'xticklabel'))
                            set(ax,'xticklabel','')
                        case {'X-Time-Val','X-Time-Val'}
                            set(ax,'yticklabelmode','auto','ytickmode','auto');
                            tick(ax,'y','autodate');
                        case {'Lon-Lat','Lon-Lat-Val','Lon-Lat-Z'}
                            set(ax,'xticklabelmode','auto','xtickmode','auto','yticklabelmode','auto','ytickmode','auto');
                            tick(ax,'x','longitude');
                            tick(ax,'y','latitude');
                            da=get(ax,'dataaspectratio');
                            ylimv=get(ax,'ylim');
                            if ylimv(1)<-90
                                ylimv(1)=-90;
                                if ylimv(2)<=ylimv(1)
                                    ylimv(2)=-89;
                                end
                            end
                            if ylimv(2)>90
                                ylimv(2)=90;
                                if ylimv(1)>=ylimv(2)
                                    ylimv(1)=89;
                                end
                            end
                            lat=mean(ylimv);
                            da(2)=cos(lat*pi/180)*da(1);
                            set(ax,'dataaspectratio',da,'ylim',ylimv)
                    end
                end
            end
            
        case 'zoomin'
            v72 = matlabversionnumber>=7.02;
            switch zoom(gcbf,'getmode')
                case {'off' 'out'}
                    zoom(gcbf,'inmode');
                    if v72
                        %Disable listeners
                        mmgr = uigetmodemanager(gcbf);
                        set(mmgr.WindowListenerHandles,'Enable','off');
                    end
                    set(gcbf,'windowbuttondownfcn','d3d_qp zoomdown');
                    if v72
                        set(mmgr.WindowListenerHandles,'Enable','on');
                    end
                case {'in' 'on'}
                    zoom(gcbf,'off');
            end
            
        case 'zoomout'
            v72 = matlabversionnumber>=7.02;
            switch zoom(gcbf,'getmode')
                case {'on' 'off' 'in'}
                    zoom(gcbf,'outmode');
                    if v72
                        %Disable listeners
                        mmgr = uigetmodemanager(gcbf);
                        set(mmgr.WindowListenerHandles,'Enable','off');
                    end
                    set(gcbf,'windowbuttondownfcn','d3d_qp zoomdown');
                    if v72
                        set(mmgr.WindowListenerHandles,'Enable','on');
                    end
                case 'out'
                    zoom(gcbf,'off');
            end
            %  putdowntext('zoomout',gcbo)
            
            
        case 'rotate3d'
            rotate3d(gcbf,get(gcbo,'State'));
            %  putdowntext('rotate3d',gcbo)
            
        case 'editborder'
            Brdr=findobj(gcbf,'type','axes','tag','border');
            if ~isempty(Brdr)
                md_paper('edit',Brdr)
            end
            
        case {'closefigure','savefigure','saveasfigure','printfigure','clipbitmap','clipmeta'}
            Fig=[];
            if isempty(UD) % if quickplot is not active do not activate it ...
                Fig=gcbf;
            elseif isempty(gcbf) || isequal(get(gcbf,'handlevisibility'),'off')
                FigIDs=get(UD.PlotMngr.FigList,'userdata');
                FigVal=get(UD.PlotMngr.FigList,'value');
                if FigVal<=length(FigIDs)
                    Fig=FigIDs(FigVal);
                end
            else
                Fig=gcbf;
            end
            if ishandle(Fig)
                args={};
                switch cmd
                    case 'closefigure'
                        AllObj=findall(Fig);
                        set(AllObj,'deletefcn','');
                        delete(Fig);
                        if ~isempty(UD) % if quickplot is not active do not activate it ...
                            d3d_qp refreshfigs
                        end
                    case {'savefigure','saveasfigure'}
                        fullfilename=get(Fig,'filename');
                        set(Fig,'menubar','figure','closerequestfcn','closereq')
                        cbar=findall(Fig,'deletefcn','qp_colorbar delete');
                        set(cbar,'deletefcn','colorbar(''delete'')')
                        if strcmp(cmd,'savefigure') && ~isempty(fullfilename)
                            saveas(Fig,fullfilename)
                        elseif ~isempty(cmdargs)
                            saveas(Fig,cmdargs{1})
                        else
                            [p,f,e]=fileparts(fullfilename);
                            if isempty(f)
                                f=listnames(Fig,'showType','no','showHandle','no','showTag','no');
                                f=str2file(f{1});
                                if isempty(f)
                                    f='untitled';
                                end
                            end
                            e = '.fig';
                            f=[f e];
                            if ~isempty(p)
                                f=fullfile(p,f);
                            else
                                p = qp_settings('figuredir');
                                if ~isempty(p)
                                    f=fullfile(p,f);
                                end
                            end
                            [newfile, newpath] = uiputfile(f, 'Save As');
                            if ischar(newfile)
                                qp_settings('figuredir',newpath)
                                [p,f,e] = fileparts(newfile);
                                if isempty(e)
                                    newfile = [newfile '.fig'];
                                end
                                fullfilename=fullfile(newpath,newfile);
                                saveas(Fig,fullfilename);
                                set(Fig,'filename',fullfilename);
                            end
                        end
                        set(Fig,'menubar','none','closerequestfcn','d3d_qp closefigure')
                        set(cbar,'deletefcn','qp_colorbar delete')
                    case {'printfigure','clipbitmap','clipmeta'}
                        if ~strncmp(computer,'PC',2) && ~isempty(strmatch(cmd,{'clipbitmap','clipmeta'}))
                            ui_message('error','Function only available on Windows PC.')
                        elseif isstandalone && matlabversionnumber<6
                            ui_message('error','Printing not available in this standalone version.')
                        else
                            sld=findobj(Fig,'tag','animslid');
                            psh=findobj(Fig,'tag','animpush');
                            set(sld,'vis','off')
                            set(psh,'vis','off')
                            switch cmd
                                case 'clipbitmap'
                                    I.PrtID='Bitmap to clipboard';
                                    I.Method=2;
                                    I.AllFigures=1;
                                    I.DPI=72;
                                    I.InvertHardcopy=0;
                                    md_print(Fig,I)
                                case 'clipmeta'
                                    I.PrtID='Metafile to clipboard';
                                    I.Method=1;
                                    I.AllFigures=1;
                                    I.InvertHardcopy=1;
                                    md_print(Fig,I)
                                otherwise
                                    if ~isempty(cmdargs)
                                        I.PrtID=cmdargs{2};
                                        I.Method=cmdargs{3};
                                        I.DPI=cmdargs{4};
                                        I.AllFigures=1;
                                        I.Color=cmdargs{5};
                                        if length(cmdargs)>5
                                            I.InvertHardcopy=cmdargs{6};
                                        else
                                            I.InvertHardcopy=1;
                                        end
                                        args={I cmdargs{1}};
                                    else
                                        I.PrtID=qp_settings('print_ID');
                                        I.Method=qp_settings('print_method');
                                        I.DPI=qp_settings('print_DPI');
                                        I.AllFigures=0;
                                        I.Color=qp_settings('print_colour');
                                        I.InvertHardcopy=qp_settings('print_inverthardcopy');
                                        I.SelectFrom=get_nondialogs;
                                        [I,FigNew]=md_print('getsettings',Fig,I);
                                        if ~isequal(Fig,FigNew)
                                            set(sld,'vis','on')
                                            set(psh,'vis','on')
                                            Fig=FigNew;
                                            sld=findobj(Fig,'tag','animslid');
                                            psh=findobj(Fig,'tag','animpush');
                                            set(sld,'vis','off')
                                            set(psh,'vis','off')
                                        end
                                        I=rmfield(I,'SelectFrom');
                                        args={I};
                                    end
                                    if ~isequal(I.PrtID,0)
                                        [I,filename]=md_print(Fig,args{:});
                                    end
                                    if isequal(I.PrtID,0) || ~ischar(filename)
                                        set(sld,'vis','on')
                                        set(psh,'vis','on')
                                        return
                                    end
                                    qp_settings('print_ID',I.PrtID);
                                    qp_settings('print_method',I.Method);
                                    qp_settings('print_DPI',I.DPI);
                                    qp_settings('print_colour',I.Color);
                                    qp_settings('print_inverthardcopy',I.InvertHardcopy);
                                    args={filename  I.PrtID  I.Method  I.DPI  I.Color  I.InvertHardcopy};
                            end
                            set(sld,'vis','on')
                            set(psh,'vis','on')
                        end
                end
                if logfile
                    writelog(logfile,logtype,cmd,args{:});
                end
            end
            
        case 'run'
            blockcomment=0;
            if isempty(cmdargs)
                [fn,pn]=uigetfile('*.qplog;*.m');
                if ischar(fn)
                    runfil=fopen([pn fn],'r');
                else
                    runfil=-1;
                end
            else
                runfil=fopen(cmdargs{1},'r');
            end
            cmdargs={};
            if runfil<0
                cmd='';
            end
            if ~isempty(cmd)
                [pn,fn,ex]=fileparts(fopen(runfil));
                runningtype=lower(ex(2:end));
                logfilerun=findobj(mfig,'tag','run','type','uimenu');
                if ~isempty(get(logfilerun,'callback'))
                    set(logfilerun,'callback','');
                    OtherLog=uimenu(logfilerun,'label','&other ...','separator','on','callback','d3d_qp run');
                else
                    OtherLog=findobj(logfilerun,'label','&other ...');
                end
                filename=fopen(runfil);
                c=setdiff(get(logfilerun,'children'),OtherLog);
                LogCallBack=sprintf('d3d_qp run ''%s''',filename);
                LogNr=10;
                LogId=[];
                set(OtherLog,'visible','off')
                for i=1:length(c)
                    Str=get(c(i),'callback');
                    if strcmp(Str,LogCallBack),
                        Str=get(c(i),'label');
                        LogId=i;
                        LogNr=Str(2)-48;
                        break
                    end
                end
                for i=1:length(c)
                    Str=get(c(i),'label');
                    Nr=Str(2)-48;
                    if Nr<LogNr
                        if strcmp(Str(2),'9')
                            delete(c(i));
                        else
                            Str(2)=Str(2)+1;
                            set(c(i),'label',Str);
                        end
                    end
                end
                if isempty(LogId)
                    uimenu(logfilerun,'label',sprintf('&1 %s',abbrevfn(filename)),'separator','off','callback',LogCallBack,'position',1);
                else
                    set(c(LogId),'label',sprintf('&1 %s',abbrevfn(filename)),'position',1);
                end
                set(OtherLog,'visible','on')
            end
            while ~isempty(cmd)
                cmdstr='';
                while isempty(cmdstr) || ismember(cmdstr(1),'%*#') || blockcomment
                    if length(cmdstr)>1 && ismember(cmdstr(1),'%*#')
                        if cmdstr(2)=='['
                            blockcomment=1;
                        elseif cmdstr(2)==']'
                            blockcomment=0;
                        end
                    end
                    cmdstr=fgetl(runfil);
                    if ~ischar(cmdstr)
                        break
                    end
                end
                cmdargs={};
                if ischar(cmdstr) && ~isempty(cmdstr)
                    switch runningtype
                        case 'm'
                            try
                                eval(cmdstr)
                            catch
                                errmsg=multiline(lasterr,'cell');
                                ui_message('error',errmsg)
                                fclose(runfil);
                                cmd='';
                            end
                        otherwise
                            [cmd,cmdargs]=qp_cmdstr(cmdstr);
                            if ~isempty(cmd)
                                if strcmp(cmd,'run')
                                    d3d_qp(cmd,cmdargs{:});
                                else
                                    try
                                        d3d_qp(cmd,cmdargs{:});
                                    catch
                                        errmsg=multiline(lasterr,'cell');
                                        if isstandalone
                                            i=1;
                                        else
                                            i=2;
                                        end
                                        ui_message('error',errmsg(i:end))
                                    end
                                end
                            end
                    end
                else
                    fclose(runfil);
                    cmd='';
                end
            end
            
        case 'closeallfig'
            h=allchild(0);
            for i=1:length(h)
                hUD=get(h(i),'userdata');
                if isstruct(hUD) && isfield(hUD,'ProgID')
                    if strcmp(hUD.ProgID,'QuickPlot')
                        delete(h(i));
                    end
                end
            end
            d3d_qp refreshfigs
            
        case {'hselectiontype','vselectiontype','hselectiontype*'}
            updategridview=1;
            if cmd(end)=='*'
                updategridview=0;
                cmd=cmd(1:end-1);
            end
            MW=UD.MainWin;
            if cmd(1)=='h'
                MWSelType = MW.HSelType;
            else
                MWSelType = MW.VSelType;
            end
            if ~isempty(cmdargs)
                news = cmdargs{1};
                ops = get(MWSelType,'string');
                newi = strmatch(news,ops,'exact');
                if length(newi)~=1
                    newi = strmatch(news,ops);
                    if length(newi)~=1
                        newi = get(MWSelType,'value');
                    end
                end
            else
                newi = get(MWSelType,'value');
            end
            set(MWSelType,'value',newi)
            news = getvalstr(MWSelType);
            switch news
                case 'M range and N range'
                    set([MW.MN MW.EditMN],'visible','off')
                    set([MW.XY MW.EditXY],'visible','off')
                    set([MW.M MW.AllM MW.EditM MW.MaxM],'visible','on')
                    set([MW.N MW.AllN MW.EditN MW.MaxN],'visible','on')
                case '(M,N) point/path'
                    set([MW.M MW.AllM MW.EditM],'visible','off')
                    set([MW.N MW.AllN MW.EditN],'visible','off')
                    set([MW.XY MW.EditXY],'visible','off')
                    set([MW.MN MW.EditMN],'visible','on')
                    set([MW.MaxM MW.MaxN],'visible','on')
                case '(X,Y) point/path'
                    set([MW.M MW.AllM MW.EditM MW.MaxM],'visible','off')
                    set([MW.N MW.AllN MW.EditN MW.MaxN],'visible','off')
                    set([MW.MN MW.EditMN],'visible','off')
                    set([MW.XY MW.EditXY],'visible','on')
                case 'K range'
                    set([MW.Z MW.EditZ],'visible','off')
                    set([MW.K MW.AllK MW.EditK MW.MaxK],'visible','on')
                case {'Z slice','dZ below surface','dZ above bed'}
                    set([MW.K MW.AllK MW.EditK MW.MaxK],'visible','off')
                    set([MW.Z MW.EditZ],'visible','on')
                    set(MW.Z,'string',strtok(news))
            end
            
            d3d_qp updateoptions
            if updategridview
                d3d_qp('gridview_update');
            end
            if logfile
                wrcmd=cmd;
                if cmd(end)=='*'
                    wrcmd=cmd(1:end-1);
                end
                writelog(logfile,logtype,wrcmd,news);
            end
            
        case {'m,n','(mn)','(xy)','m,n*','(mn)*','(xy)*','k','z'}
            if cmd(end)=='*'
                star='*';
                cmd=cmd(1:end-1);
            else
                star='';
            end
            switch cmd
                case 'm,n'
                    d3d_qp(['hselectiontype' star],'M range and N range');
                case '(mn)'
                    d3d_qp(['hselectiontype' star],'(M,N) point/path');
                case '(xy)'
                    d3d_qp(['hselectiontype' star],'(X,Y) point/path');
                case 'k'
                    d3d_qp('vselectiontype','K range');
                case 'z'
                    d3d_qp('vselectiontype','Z slice');
            end
            
        case 'reset'
            if logfile
                writelog(logfile,logtype,cmd);
            end
            MW=UD.MainWin;
            State=qp_state_startup;
            set(MW.T,'enable','off','UserData',[])
            set(MW.AllT,'enable','off','value',0,'UserData',[])
            set(MW.EditT,'enable','off','string','','backgroundcolor',Inactive,'UserData',[])
            set(MW.MaxT,'enable','off','string','( - )','UserData',1)
            set(MW.ShowT,'enable','off','value',0,'UserData',[])
            set(MW.TList,'enable','off','value',[],'string','','UserData',0)
            set(MW.S,'enable','off','UserData',[])
            set(MW.AllS,'enable','off','value',0,'UserData',[],'visible','off')
            set(MW.EditS,'enable','off','string','','backgroundcolor',Inactive,'UserData',[],'visible','off')
            set(MW.MaxS,'enable','off','string','( - )','UserData',1,'visible','off')
            set(MW.StList,'enable','off','value',1,'string',' ','backgroundcolor',Inactive,'UserData',[],'visible','on')
            set(MW.Stat,'visible','off')
            set(MW.HSelType,'String',{'M range and N range','(M,N) point/path','(X,Y) point/path'},'value',1)
            set(MW.VSelType,'String',{'K range','Z slice','dZ below surface','dZ above bed'},'value',1)
            set(MW.MN,'visible','off')
            set(MW.EditMN,'string','','Userdata',[],'visible','off')
            set(MW.XY,'visible','off')
            set(MW.EditXY,'string','','Userdata',[],'visible','off')
            set(MW.M,'enable','off','UserData',[],'visible','on')
            set(MW.AllM,'enable','off','value',1,'UserData',[],'visible','on')
            set(MW.EditM,'enable','off','string','','backgroundcolor',Inactive,'UserData',[],'visible','on')
            set(MW.MaxM,'enable','off','string','( - )','UserData',1,'visible','on')
            set(MW.N,'enable','off','UserData',[],'visible','on')
            set(MW.AllN,'enable','off','value',1,'UserData',[],'visible','on')
            set(MW.EditN,'enable','off','string','','backgroundcolor',Inactive,'UserData',[],'visible','on')
            set(MW.MaxN,'enable','off','string','( - )','UserData',1,'visible','on')
            set(MW.K,'enable','off','UserData',[],'visible','on')
            set(MW.AllK,'enable','off','value',1,'UserData',[],'visible','on')
            set(MW.EditK,'enable','off','string','','backgroundcolor',Inactive,'UserData',[],'visible','on')
            set(MW.MaxK,'enable','off','string','( - )','UserData',1,'visible','on')
            set(MW.Z,'visible','off')
            set(MW.EditZ,'string','','Userdata',[],'visible','off')
            set(MW.LoadData,'enable','off')
            set(MW.DefVar,'enable','off')
            set(MW.QuickV,'enable','off')
            set(MW.Add2Plot,'enable','off')
            set(UOH,'enable','off','visible','off','backgroundcolor',Inactive)
            set(findobj(UOH,'tag','plotcoordinate=?'),'value',1,'string',{' '})
            set(findobj(UOH,'tag','component=?'),'value',1,'string',{' '})
            set(findobj(UOH,'tag','numformat=?'),'string','%.2f')
            set(findobj(UOH,'tag','fontsize=?'),'userdata',6,'string','6')
            set(findobj(UOH,'tag','horizontalalignment=?'),'value',2)
            set(findobj(UOH,'tag','verticalalignment=?'),'value',3)
            set(findobj(UOH,'tag','textbox=?'),'value',0)
            set(findobj(UOH,'tag','textboxfacecolour=?'),'backgroundcolor',State.textboxfacecolour,'userdata',State.textboxfacecolour)
            set(findobj(UOH,'tag','vecscalem=?'),'value',1)
            set(findobj(UOH,'tag','vectorstyle=?'),'value',1)
            set(findobj(UOH,'tag','1vecunit=?'),'userdata',1,'string','1')
            
            set(findobj(UOH,'tag','linewidth=?'),'userdata',0.5,'string','0.5')
            set(findobj(UOH,'tag','vertscalem=?'),'value',1)
            set(findobj(UOH,'tag','vscale=?'),'userdata',1,'string','1')
            set(findobj(UOH,'tag','presenttype=?'),'value',1,'string',{' '})
            set(findobj(UOH,'tag','thinfld=?'),'value',1)
            set(findobj(UOH,'tag','thinfact=?'),'userdata',1,'string','1')
            set(findobj(UOH,'tag','thindist=?'),'userdata',50,'string','50')
            set(findobj(UOH,'tag','colourvectors'),'value',0)
            set(findobj(UOH,'tag','colourdams'),'value',0)
            set(findobj(UOH,'tag','vectorcolour=?'),'value',1,'string',{' '})
            set(findobj(UOH,'tag','colclassify'),'value',0)
            set(findobj(UOH,'tag','thresholds=?'),'string','')
            set(findobj(UOH,'tag','threshdistr=?'),'value',1)
            set(findobj(UOH,'tag','climmode=?'),'value',1)
            set(findobj(UOH,'tag','climmax=?'),'userdata',1,'string','1')
            set(findobj(UOH,'tag','climmax=?'),'userdata',0,'string','0')
            set(findobj(UOH,'tag','colour=?'),'backgroundcolor',State.colour,'userdata',State.colour)
            set(findobj(UOH,'tag','fillpolygons'),'value',0)
            set(findobj(UOH,'tag','facecolour=?'),'backgroundcolor',State.facecolour,'userdata',State.facecolour)
            L=set(mfig,'defaultlinelinestyle');
            Li=strmatch('-',L,'exact');
            if isempty(Li)
                Li=1;
            end
            set(findobj(UOH,'tag','linestyle=?'),'value',Li)
            L=set(mfig,'defaultlinemarker');
            Li=strmatch('none',L,'exact');
            if isempty(Li)
                Li=1;
            end
            set(findobj(UOH,'tag','marker=?'),'value',Li)
            set(findobj(UOH,'tag','usemarkercolour'),'value',0)
            set(findobj(UOH,'tag','markercolour=?'),'backgroundcolor',State.markercolour,'userdata',State.markercolour)
            set(findobj(UOH,'tag','usemarkerfillcolour'),'value',0)
            set(findobj(UOH,'tag','markerfillcolour=?'),'backgroundcolor',State.markerfillcolour,'userdata',State.markerfillcolour)
            d3d_qp('colourmap*','jet')
            set(findobj(UOH,'tag','colourbar'),'value',1)
            set(findobj(UOH,'tag','colbarhorz'),'value',0)
            set(findobj(UOH,'tag','clippingvals=?'),'userdata',-999,'string','-999')
            set(findobj(UOH,'tag','exporttype=?'),'value',1,'string',{' '})
            
            d3d_qp updatedomains
            
        case 'hideversion'
            set(mfig,'name','Delft3D-QUICKPLOT')
            
        case 'showversion'
            set(mfig,'name',cat(2,'Delft3D-QUICKPLOT ',qpversion))
            
        case 'validation'
            qp_validate(cmdargs{:});
            
        case {'preferences','prefpane', ...
                'defaultnewfigure','defaultloadfigure','defaultfigure', ...
                'gridviewbackgroundcolor','gridviewgridcolor', ...
                'gridviewselectioncolor','gridviewlandboundarycolor', ...
                'defaultfigurecolor','gridviewshowindices','changefont', ...
                'defaultaxescolor','boundingbox','v6zoombehavior'}
            qp_prefs(UD,mfig,cmd,cmdargs);
            
        case 'deltaresweb'
            ops={};
            if matlabversionnumber>5
                ops={'-browser'};
            end
            site='http://www.deltaressystems.com';
            try
                stat=web(site,ops{:});
            catch
                stat=1;
            end
            if stat==1
                ui_message('error',['Could not find browser to visit ',site,'.'])
            elseif stat==2
                ui_message('error',['Could not start browser to visit ',site,'.'])
            end
            
        otherwise
            cmd2 = cmd;
            cmd = -1;
    end
catch
    ui_message('error',{sprintf('Catch in d3d_qp\\%s:',cmd),lasterr})
end
if ~ischar(cmd)
    try
        ui_message('error',sprintf('Unknown command in d3d_qp: %s',cmd2))
    catch
        ui_message('error','Unknown command in d3d_qp: <cmd2 not defined>')
    end
end
