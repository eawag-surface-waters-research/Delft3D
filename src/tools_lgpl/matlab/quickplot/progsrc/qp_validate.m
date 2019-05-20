function qp_validate(varargin)
%QP_VALIDATE Helper function to validate Delft3D-QUICKPLOT.

%----- LGPL --------------------------------------------------------------------
%
%   Copyright (C) 2011-2019 Stichting Deltares.
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

log_style('latex')
baseini='validation.ini';
val_dir = '';
openlog = true;

i = 0;
while i<nargin
    i = i+1;
    switch lower(varargin{i})
        case 'openlog'
            i = i+1;
            openlog = varargin{i};
        case {'latex','html'}
            log_style(lower(varargin{i}))
        otherwise
            val_dir = varargin{i};
    end
end
if isempty(val_dir)
    %
    % Would prefer to use uigetdir, but that doesn't compile.
    %
    [~,val_dir]=uigetfile(baseini,'Select directory containing validation cases');
    if ~ischar(val_dir)
        return
    end
end

if ~isstandalone
    qp_path=which('d3d_qp');
    qp_path=fileparts(qp_path);
    addpath(qp_path)
end

currdir=pwd;
TC1 = 1;
includes = {};
logid1=-1;
switch log_style
    case 'latex'
        Color.Page       = 'white';
        Color.Titlebar   = 'magenta!25!cyan!50';
        Color.Table      = {'magenta!10!cyan!20' 'magenta!5!cyan!10'};
        Color.Failed     = 'red!80!black';
        Color.Success    = 'green!80!black';
        Color.Font       = 'black';
        %
        logname='validation_log.tex';
        sc={['\textcolor{',Color.Failed,'}{Failed}']
            ['\textcolor{',Color.Success,'}{Successful}']};
        sc3={['\textcolor{',Color.Failed,'}{Failed}']
            ['\textcolor{',Color.Success,'}{Successful}']
            ['\textcolor{',Color.Success,'}{Successful}']};
    otherwise
        Color.Page       = 'FFFFFF';
        Color.Titlebar   = 'BAD4F4';
        Color.Table      = {'DEEBFA' 'E8F1FB'};
        Color.Failed     = 'FF0000';
        Color.Success    = '00AA00';
        Color.Font       = '000000';
        %
        logname='validation_log.html';
        sc={['<font color=',Color.Failed,'>Failed</font> (open <a href="reference">reference folder</a>)']
            ['<font color=',Color.Success,'>Successful</font>']};
        sc3={['<font color=',Color.Failed,'>Failed</font> (open <a href="reference">reference folder</a>)']
            ['<font color=',Color.Success,'>Successful</font> (open <a href="reference">reference folder</a>)']
            ['<font color=',Color.Success,'>Successful</font>']};
end
AnyFail=0;
NTested=0;
NFailed=0;
fs=filesep;
sdata=['..',fs,'data',fs];
sref=['..',fs,'reference',fs];
swrk=['..',fs,'work',fs];
slog=['..',fs,'logfiles',fs];
T_=1; ST_=2; M_=3; N_=4; K_=5;
%
Hpb=progressbar('cancel','delete(gcbf)','pause','on');
t_init = now;
pHpb=get(Hpb,'position');
ssz=qp_getscreen;
set(Hpb,'position',[ssz(1)+10 ssz(2)+ssz(4)-pHpb(4)-30 pHpb(3) pHpb(4)])
%
UserInterrupt=0;
if matlabversionnumber>=7.02
    saveops={'-v6' '-mat'};
elseif matlabversionnumber>=7
    saveops={'-NOUNICODE' '-mat'};
else
    saveops={'-mat'};
end
%
% Allow for a large number of messages
%
ui_message('max',10000)
set(findall(0,'tag','UI_MESSAGE window'),'position',[ssz(1)+10 ssz(2)+40 pHpb(3) ssz(4)-pHpb(4)-120])
DefFigProp.defaultfigure = qp_settings('defaultfigure',-999);
qp_settings('defaultfigure','')
DefFigProp.defaultfigurecolor = qp_settings('defaultfigurecolor',-999);
qp_settings('defaultfigurecolor',get(0,'factoryuicontrolbackgroundcolor')*255)
DefFigProp.defaultaxescolor = qp_settings('defaultaxescolor',-999);
qp_settings('defaultaxescolor',[255 255 255])
DefFigProp.boundingbox = qp_settings('boundingbox',-999);
qp_settings('boundingbox',0)
current_procdef='';
try
    full_ln=fullfile(val_dir,logname);
    if ~localexist(fullfile(val_dir,baseini))
        ui_message('error','Invalid validation directory: ''%s'' is missing.',baseini)
        if ishandle(Hpb)
            delete(Hpb);
        end
        return
    end
    logid1=fopen(full_ln,'w');
    if logid1<0
        ui_message('error',{'Cannot open logfile for validation report.','Stopping validation process.'})
        return
    else
        t1 = write_header(logid1,'MAIN',Color);
    end
    cd(val_dir)
    d=dir('*');
    for i=length(d):-1:1
        if ~d(i).isdir || strcmp(d(i).name(1),'.') || strcmp(d(i).name(1),'..')
            d(i)=[];
        end
    end
    %
    % Sorting required for MATLAB 5.3 ...
    %
    [~,I]=sort(upper({d.name}));
    d=d(I);
    [d.dt] = deal(NaN);
    %
    % Start up the QuickPlot interface such that this time is not included
    % in the timing of the first testcase. Hide the plot manager because
    % this dialog affects the timing.
    %
    d3d_qp('hideplotmngr');
    new_procdef = fullfile(val_dir,'proc_def.dat');
    if exist(new_procdef,'file')
        current_procdef = qp_settings('delwaq_procdef');
        qp_settings('delwaq_procdef',new_procdef)
    end
    %
    sumt = 0;
    numt = 0;
    for i=1:length(d)
        timid = fopen(fullfile(val_dir,d(i).name,'reference','timing.txt'));
        if timid>0
            [dt2,cnt] = fscanf(timid,'%f',1);
            if cnt==1
                d(i).dt = dt2;
                sumt = sumt + dt2;
                numt = numt + 1;
            end
            fclose(timid);
        end
    end
    %
    if numt>0
        avg_dt = sumt/numt;
    else
        avg_dt = 4; % estimate of average time based on 440 test cases
    end
    case_dt = [d.dt];
    case_dt(isnan(case_dt)) = avg_dt;
    acc_dt = (now-t_init)*86400;
    tot_dt = max(1,acc_dt + sum(case_dt)); % at least a second
    %
    includes = repmat({''},length(d),2);
    for i=1:length(d)
        progressbar(acc_dt/tot_dt,Hpb,'title',d(i).name);
        ui_message('',['Case: ',d(i).name])
        includes{i,1} = [d(i).name '/'];
        includes{i,2} = logname;
        NTested=NTested+1;
        DiffFound=0;
        color    = '';
        frcolor  = Color.Success; 
        lgcolor  = Color.Font;
        result   = '';
        frresult = 'PASSED';
        lgresult = 'N/A';
        logid2=[];
        TC2=1;
        Crash = [];
        try
            cd(fullfile(val_dir,d(i).name));
            CaseInfo='case.ini';
            if isempty(dir('data'))
                f = dir('*');
                ensure_directory('data');
                for ii=1:length(f)
                    if ~strcmp(f(ii).name,'.') && ~strcmp(f(ii).name,'..')
                        movefile(f(ii).name,'data');
                    end
                end
            end
            dd=dir(['data' fs '*']);
            if ~localexist(CaseInfo) && length(dd)>2
                CID=inifile('new');
                CID=inifile('set',CID,'','FileName',dd(3).name);
                CID=inifile('set',CID,'logfilecheck','example.qplog','example.png');
                inifile('write',CaseInfo,CID);
            end
            if localexist(CaseInfo)
                CaseInfo=inifile('open',CaseInfo);
                logid2=fopen(logname,'w');
                dt2_old = d(i).dt;
                t2 = write_header(logid2,d(i).name,Color);
                emptyTable2 = true;
                %
                % check for log files to run ...
                %
                if isempty(dir('logfiles'))
                    ensure_directory('logfiles');
                end
                logs=dir(['logfiles' fs '*.qplog']);
                logm=dir(['logfiles' fs '*.m']);
                logs=cat(1,logs,logm);
                %
                % check for reference directory...
                %
                if isempty(dir('reference'))
                    ensure_directory('reference');
                end
                %
                % Switch to work directory...
                %
                if isempty(dir('work'))
                    ensure_directory('work');
                end
                cd('work')
                workdir=pwd;
                %
                % Delete all files in work directory (but make sure that we actually are in the work directory) ...
                %
                if ~isequal(workdir(end-3:end),'work')
                    write_log(logid2,'No work directory?');
                    error('No work directory?')
                end
                delete('*')
                FileName=inifile('get',CaseInfo,'*','FileName','');
                FileName2=inifile('get',CaseInfo,'*','FileName2','');
                if isempty(FileName2)
                    FileName2={};
                    write_section(logid2,'Opening ''%s''',protected(FileName));
                else
                    write_section(logid2,'Opening ''%s'' with ''%s''',protected(FileName),protected(FileName2));
                    FileName2={[sdata,FileName2]};
                end
                if length(FileName)<7 || ~isequal(lower(FileName(1:7)),'http://')
                    FileName = [sdata,FileName];
                end
                d3d_qp('openfile',FileName,FileName2{:});
                FI=qpfile;
                if ~isstruct(FI)
                    error('Error opening file.');
                end
                [ChkOK,Dms]=qp_getdata(FI,'domains');
                write_log(logid2,'Reading domains: %s',sc{ChkOK+1});
                if isempty(Dms)
                    Dms={'no name'};
                end
                dmx=length(Dms);
                Props={};
                for dm=1:dmx
                    [Chk,P]=qp_getdata(FI,dm);
                    write_log(logid2,'Reading fields of domain ''%s'': %s',protected(Dms{dm}),sc{Chk+1});
                    ChkOK=Chk&ChkOK;
                    for p=1:length(P)
                        if strcmp(P(p).Name,'-------')
                            P(p).Size=[0 0 0 0 0];
                        else
                            [Chk,P(p).Size]=qp_getdata(FI,dm,P(p),'size');
                            drawnow
                            ChkOK=Chk&ChkOK;
                        end
                    end
                    Props{dm}=P;
                end
                write_log(logid2,'');
                if ChkOK
                    CmpFile='datafields.mat';
                    RefFile=[sref CmpFile];
                    WrkFile=[swrk CmpFile];
                    if localexist(RefFile)
                        cmpFile=localload(RefFile);
                        if isfield(cmpFile,'Props')
                            PrevProps=cmpFile.Props;
                        else
                            PrevProps=cmpFile.Data;
                        end
                        DiffFound = vardiff(Props,PrevProps)>1;
                        write_log(logid2,'Comparing new and old fields:');
                        if DiffFound
                            DiffMessage = 1; % Failed
                            localsave(WrkFile,Props,saveops);
                            if length(Props)~=length(PrevProps)
                                write_log(logid2,'Number of domains differs.');
                                write_log(logid2,'Reference data set contains %i domains.',length(PrevProps));
                                write_log(logid2,'New data set contains %i domains.',length(Props));
                            else
                                JustAddedData=1;
                                for dm=1:length(Props)
                                    Prop = Props{dm};
                                    PropRef = PrevProps{dm};
                                    pn={Prop.Name};
                                    ppn={PropRef.Name};
                                    dpn=setdiff(pn,ppn);
                                    dppn=setdiff(ppn,pn);
                                    if length(Props)>1
                                        write_log(logid2,'<b>Domain ''%s''</b>',Dms{dm});
                                    end
                                    if ~isempty(dpn)
                                        write_log(logid2,'New datafields:');
                                        write_log(logid2,'<li>%s</li>',dpn{:});
                                        if ~isempty(dppn)
                                            write_log(logid2,'Deleted datafields:');
                                            write_log(logid2,'<li>%s</li>',dppn{:});
                                        end
                                        [~,ipn,ippn]=intersect(pn,ppn);
                                        Prop=Prop(ipn);
                                        PropRef=PropRef(ippn);
                                    elseif ~isempty(dppn)
                                        JustAddedData=0;
                                        write_log(logid2,'Deleted datafields:');
                                        write_log(logid2,dppn);
                                        [~,ipn,ippn]=intersect(pn,ppn);
                                        Prop=Prop(ipn);
                                        PropRef=PropRef(ippn);
                                    end
                                    %
                                    if vardiff(Prop,PropRef)>1
                                        JustAddedData=0;
                                        vardiff(Prop,PropRef,logid2,log_style,'Current Data','Reference Data');
                                    end
                                    drawnow
                                end
                                if JustAddedData
                                    DiffMessage = 2; % Successful but still option to open reference folder
                                    DiffFound = 0;
                                    if strcmp(frcolor,Color.Success)
                                        frcolor=Color.Font;
                                    end
                                end
                            end
                        else
                            write_log(logid2,'No differences.');
                            DiffMessage = 3; % Successful
                        end
                        write_log(logid2,'Conclusion: %s',sc3{DiffMessage});
                    else
                        localsave(RefFile,Props,saveops);
                    end
                    if DiffFound
                        frcolor=Color.Failed;
                        frresult='FAILED: Data fields differ.';
                        ChkOK=0;
                    end
                else
                    frcolor=Color.Failed;
                    frresult='FAILED: Error while reading data.';
                    ChkOK=0;
                end
                %
                NP=length(P);
                NL=length(logs);
                NT=NP+NL;
                if 1%ChkOK
                    write_begin_table(logid2,Color)
                    datacheck=inifile('get',CaseInfo,'datacheck','default',1);
                    P=Props{dmx};
                    for p=1:NP
                        if progressbar((acc_dt+case_dt(i)*(p-1)/NT)/tot_dt,Hpb)<0
                            write_table2_line(logid2,Color.Table{TC2},'','',''); % at least one line needed in table
                            write_end_table(logid2,false,emptyTable2);
                            UserInterrupt=1;
                            error('User interrupt');
                        end
                        if ~strcmp(P(p).Name,'-------')
                            if P(p).NVal<0
                                write_table2_line(logid2,Color.Table{TC2},P(p).Name,'Check n/a','');
                                emptyTable2 = false;
                                TC2=3-TC2;
                            elseif ~inifile('get',CaseInfo,'datacheck',P(p).Name,datacheck)
                                write_table2_line(logid2,Color.Table{TC2},P(p).Name,'Skipped','Skipped');
                                emptyTable2 = false;
                                TC2=3-TC2;
                            else
                                PName=P(p).Name;
                                PName_double = strmatch(PName,{P(1:p-1).Name},'exact');
                                PName=str2file(PName);
                                CmpFile=[PName '.mat'];
                                if PName_double
                                    CmpFile=[PName sprintf('.(%i).mat',PName_double+1)];
                                end
                                RefFile=[sref CmpFile];
                                WrkFile=[swrk CmpFile];
                                idx={};
                                subf={};
                                if P(p).DimFlag(ST_)
                                    idx={1};
                                    if P(p).DimFlag(T_)
                                        if P(p).DimFlag(M_) || P(p).DimFlag(N_)
                                            idx={P(p).Size(T_) idx{:}};
                                        else
                                            idx={1:min(10,P(p).Size(T_)) idx{:}};
                                        end
                                    end
                                end
                                [Chk,subfields]=qp_getdata(FI,dm,P(p),'subfields');
                                if Chk && ~isempty(subfields)
                                    subf={length(subfields)};
                                end
                                [Chk,Data]=qp_getdata(FI,dm,P(p),'griddata',subf{:},idx{:});
                                if Chk && isempty(Data)
                                    write_table2_line(logid2,Color.Table{TC2},P(p).Name,color_write('Failed to get data',Color.Failed),'');
                                    emptyTable2 = false;
                                    TC2=3-TC2;
                                    Chk = 0;
                                else
                                    write_table2_line(logid2,Color.Table{TC2},P(p).Name,sc{Chk+1},[]);
                                    emptyTable2 = false;
                                    TC2=3-TC2;
                                end
                                if ~Chk
                                    frcolor=Color.Failed;
                                    frresult=sprintf('FAILED: Error retrieving data for ''%s''.',P(p).Name);
                                    ChkOK=0;
                                else
                                    if localexist(RefFile)
                                        cmpFile=localload(RefFile);
                                        PrevData=cmpFile.Data;
                                    else
                                        PrevData=[];
                                    end
                                    if isstruct(PrevData)
                                        if isfield(Data,'TRI')
                                            Data.TRI = sortrows(sort(Data.TRI')'); %#ok<TRSRT>
                                        end
                                        if isfield(PrevData,'TRI')
                                            PrevData.TRI = sortrows(sort(PrevData.TRI')'); %#ok<TRSRT>
                                        end
                                        %
                                        DiffFound=vardiff(Data,PrevData);
                                        addedfields = {};
                                        if DiffFound==2
                                            newfields=fields(Data);
                                            oldfields=fields(PrevData);
                                            if all(ismember(oldfields,newfields))
                                                addedfields = setdiff(newfields,oldfields);
                                                newData = Data;
                                                for f = 1:length(addedfields)
                                                    newData = rmfield(newData,addedfields{f});
                                                end
                                                DiffFound=vardiff(newData,PrevData)>1;
                                                if ~DiffFound
                                                    DiffFound = -1;
                                                end
                                            else
                                                % new data structure misses some fields
                                                % that were included in the old
                                                % (reference) data structure
                                                DiffFound = 1;
                                            end
                                        else
                                            newfields='';
                                            DiffFound=DiffFound>1;
                                        end
                                        if DiffFound
                                            localsave(WrkFile,Data,saveops);
                                            write_table2_line(logid2,[],[],[],1);
                                            emptyTable2 = false;
                                            if ~isempty(addedfields)
                                                write_log(logid2,'New Fields:');
                                                write_log(logid2,addedfields);
                                                if isequal(frcolor,Color.Success) % switch to black colour only if no real error has occurred
                                                    frcolor=Color.Font;
                                                end
                                                if DiffFound>0
                                                    write_log(logid2,'');
                                                end
                                            end
                                            if DiffFound>0
                                                write_log(logid2,sc{2-DiffFound});
                                                if ~isempty(addedfields)
                                                    vardiff(newData,PrevData,logid2,'html','Current Data','Reference Data');
                                                else
                                                    vardiff(Data,PrevData,logid2,'html','Current Data','Reference Data');
                                                end
                                                frcolor=Color.Failed;
                                            end
                                            write_table2_line(logid2,[],[],[],2);
                                            ChkOK = DiffFound<=0;
                                            if ~ChkOK
                                                frresult=sprintf('FAILED: Data changed for ''%s''.',P(p).Name);
                                            end
                                            localsave([PName,'.mat'],Data,saveops);
                                        else
                                            write_table2_line(logid2,[],[],[],sc{2-DiffFound});
                                            emptyTable2 = false;
                                        end
                                    else
                                        localsave(RefFile,Data,saveops);
                                        write_table2_line(logid2,[],[],[],'Created');
                                        emptyTable2 = false;
                                        if isequal(frresult,'PASSED')
                                            frcolor=Color.Font;
                                            frresult='CREATED';
                                        end
                                    end
                                end
                            end
                        else
                            write_table_rule(logid2,Color)
                        end
                        flush(logid2)
                    end
                    write_end_table(logid2,false,emptyTable2);
                    drawnow
                end
                %
                if isempty(logs)
                    write_rule(logid2);
                    write_log(logid2,'No log files to run for validation.');
                else
                    lgcolor=Color.Success;
                    lgresult='PASSED';
                    for lg=1:NL
                        if progressbar((acc_dt+case_dt(i)*(NP+lg-1)/NT)/tot_dt,Hpb)<0
                            UserInterrupt=1;
                            error('User interrupt');
                        end
                        logf=logs(lg).name;
                        write_rule(logid2);
                        write_section(logid2,'Running log file: %s',protected(logf));
                        d3d_qp('reset');
                        d1=dir; d1={d1.name};
                        m1=ui_message('getall');
                        d3d_qp('run',[slog,logf]);
                        d2=dir; d2={d2.name};
                        m2=ui_message('getall');
                        checkfs=setdiff(d2,d1);
                        if length(m2)>length(m1)
                            diffm=m2(length(m1)+2:length(m2));
                            for mi = 1:length(diffm)
                                write_log(logid2,'%s: %s',logf,color_write(diffm{mi},Color.Failed));
                            end
                        end
                        if isempty(checkfs)
                            write_log(logid2,'No file to check.');
                            lgcolor=Color.Failed;
                            lgresult=sprintf('FAILED: No check on ''%s''.',logf);
                        else
                            %checkf=inifile('get',CaseInfo,'logfilecheck',logf,'');
                            for icheck=1:length(checkfs)
                                checkf=checkfs{icheck};
                                write_log1(logid2,'Checking File ''%s'': ',protected(checkf));
                                showfig=0;
                                [~,~,ext]=fileparts(checkf);
                                reffile=[sref,checkf];
                                args={};
                                switch lower(ext)
                                    case '.png'
                                        % matching DPI for default size: 53, 54, 56, 57 61, 62, 64, 65, 66, 69
                                        args={'skip',256};
                                        showfig=1;
                                    case '.asc'
                                        args={'skip',71};
                                    case '.mat'
                                        args={'skip',128};
                                    case '.dbf'
                                        args={'skip',16};
                                end
                                if ~localexist(reffile)
                                    copyfile(checkf,reffile);
                                    write_log(logid2,'reference file created');
                                    if isequal(lgresult,'PASSED')
                                        lgcolor=Color.Font;
                                        lgresult='CREATED';
                                    end
                                    if showfig
                                        include_figure(logid2,['reference/' checkf]);
                                    end
                                else
                                    [Eql,Msg]=filesequal(checkf,reffile,args{:});
                                    diffimg={};
                                    if ~Eql
                                        try
                                            switch lower(ext)
                                                case '.png'
                                                    I1=imread(checkf);
                                                    I2=imread(reffile);
                                                    if ~isequal(size(I1),size(I2))
                                                        Msg='The bitmap sizes are different.';
                                                    else
                                                        I1=double(I1);
                                                        I2=double(I2);
                                                        diffimg=[checkf(1:end-4) '_diff.png'];
                                                        imwrite(1-abs(I1-I2)/255,diffimg);
                                                        diffimg={['work/' diffimg]};
                                                        Msg='The bitmap images are different.';
                                                    end
                                            end
                                        catch
                                        end
                                    end
                                    if ~isempty(Msg)
                                        Msg=cat(2,': ',Msg);
                                    end
                                    write_log(logid2,[sc{1+Eql},Msg]);
                                    if Eql
                                        if showfig
                                            include_figure(logid2,['reference/' checkf]);
                                        end
                                    else
                                        if showfig
                                            include_diff_figures(logid2,{['reference/' checkf],['work/' checkf],diffimg{:}},Color);
                                        end
                                        lgcolor=Color.Failed;
                                        lgresult='FAILED: Log file results differ.';
                                    end
                                end
                            end
                        end
                        flush(logid2)
                        drawnow
                    end
                end
            else
                frcolor=Color.Font;
                frresult='FAILED: case.ini missing.';
            end
        catch Crash
            color=Color.Failed;
            AnyFail=1;
            if UserInterrupt
                result='FAILED: User interrupt.';
            else
                result='FAILED: Validation test crashed - ';
            end
        end
        d3d_qp('closefile');
        if ~isempty(logid2)
            if ~isempty(Crash)
                write_log(logid2,color_write(protected(Crash.message),Color.Failed,true));
            end
            [dt2,dt2_str] = write_footer(logid2,d(i).name,Color,t2,dt2_old);
            fclose(logid2);
            logid2=[];
            %
            if isnan(dt2_old)
                timid = fopen('../reference/timing.txt','w');
                fprintf(timid,'%5.1f',dt2);
                fclose(timid);
            end
        elseif ~isempty(Crash)
            result = [result Crash.message];
            dt2 = (now-t2)*86400;
            dt2_str = duration(dt2);
        end
        CaseFailed = ~isempty(strmatch('FAILED',frresult)) | ~isempty(strmatch('FAILED',lgresult)) | ~isempty(strmatch('FAILED',result));
        NFailed=NFailed + double(CaseFailed);
        AnyFail=AnyFail | CaseFailed;
        if AnyFail && ishandle(Hpb)
            progressbar(Hpb,'color','r')
        elseif ~ishandle(Hpb)
            UserInterrupt=1;
        end
        if ~isempty(result)
            write_table1_line(logid1,Color.Table{TC1},d(i).name,color,result,[],[],logname,dt2_str);
            TC1=3-TC1;
        else
            write_table1_line(logid1,Color.Table{TC1},d(i).name,frcolor,frresult,lgcolor,lgresult,logname,dt2_str);
            TC1=3-TC1;
        end
        flush(logid1);
        if UserInterrupt
            break
        end
        acc_dt = acc_dt + case_dt(i);
    end
catch err
    if ~isempty(logid2)
        fclose(logid2);
    end
    if logid1>0
        write_table_error(logid1,Color,err)
        AnyFail=1;
    end
end
if ishandle(Hpb)
    delete(Hpb);
end
cd(currdir)
if ~isempty(current_procdef)
    qp_settings('delwaq_procdef',current_procdef)
end
if logid1>0
    write_end_table(logid1,true);
    write_includes(logid1,includes);
    write_footer(logid1,'MAIN',Color,t1,NaN);
    fclose(logid1);
end
if AnyFail
    ui_message('error','Testbank failed on %i out of %i cases! Check log file.\n',NFailed,NTested)
    %
    if openlog
        if matlabversionnumber>5
            ops={'-browser'};
        else
            ops={};
        end
        try
            web(full_ln,ops{:});
        catch
        end
    end
else
    ui_message('','Testbank completed successfully (%i cases).\n',NTested)
end
qp_settings('defaultfigure',DefFigProp.defaultfigure)
qp_settings('defaultfigurecolor',DefFigProp.defaultfigurecolor)
qp_settings('defaultaxescolor',DefFigProp.defaultaxescolor)
qp_settings('boundingbox',DefFigProp.boundingbox)


function str = protected(str)
switch log_style
    case 'latex'
        if iscell(str)
            for i = 1:numel(str)
                str{i} = protected(str{i});
            end
        else
            str = strrep(str,'\','\char`\\');
            str = strrep(str,'_','\_');
            str = strrep(str,'#','\#');
            str = strrep(str,'&','\&');
            str = strrep(str,'%','\%');
            str = strrep(str,'�','$^3$');
        end
end


function str = makelabel(str)
switch log_style
    case 'latex'
        str = strrep(str,'&','_and_');
end



function str = protect_filename(str)
switch log_style
    case 'latex'
        str = strrep(str,' ','" "');
        %str = strrep(str,'_','\_');
end


function t = write_header(logid,casename,Color)
stalone='';
if isstandalone
    stalone=' (standalone)';
end
c = clock;
switch log_style
    case 'latex'
        if strcmp(casename,'MAIN')
            % main document
            fprintf(logid,'%s\n','\documentclass[table]{deltares_manual}');
            fprintf(logid,'%s\n','\usepackage{pdflscape}');
            versionstr = d3d_qp('version'); % returns "source code version" or "vA.B.revsion (64bit)"
            if versionstr(1)=='v'
                version = sscanf(a,'v%d.%d.%d');
                revision = version(3);
                % insert QUICKPLOT revision number into SVN revsion string of the report
                fprintf(logid,'%s%d%s%s%s\n','\svnid{$Id$}');
                versionstr = sprintf('%d.%d',version(1:2));
            end
            fprintf(logid,'\n');
            fprintf(logid,'%s\n','\begin{document}');
            fprintf(logid,'%% %s\n','\pagestyle{empty}');
            fprintf(logid,'%% %s\n','\includepdf[pages=1,offset=72 -70]{pictures/Delft3D-cover_hydro.pdf} % links-rechts past precies');
            fprintf(logid,'%% %s\n','\cleardoublepage');
            fprintf(logid,'%s%s%s\n','\pagecolor{',Color.Page,'}');
            fprintf(logid,'\n');
            fprintf(logid,'%s\n','\input{../../../../doc/user_manuals/common/program_names}');
            fprintf(logid,'\n');
            fprintf(logid,'%s\n','\title{\QUICKPLOT\ Testing}');
            fprintf(logid,'%s\n','\subtitle{Automated regression testing report}');
            fprintf(logid,'%s\n','\manualtype{Validation Document}');
            fprintf(logid,'%s\n','\distribution{}');
            fprintf(logid,'%s{%s%s}\n','\version',versionstr,stalone);
            fprintf(logid,'%s\n','\deltarestitle');
            fprintf(logid,'%s%s%s%s%s\n','\rowcolors{1}{',Color.Table{1},'}{',Color.Table{2},'}');
            fprintf(logid,'\n');
            fprintf(logid,'%s\n','\begin{landscape}');
            fprintf(logid,'%s{%s} %s{Chap:%s}\n','\chapter','Validation summary','\label','Summary');
            fprintf(logid,'%s\n','\begin{longtable}{|l|l|l|l|l|}');
            fprintf(logid,'%s\n','\hiderowcolors');
            fprintf(logid,'%s\n','\caption{Overview of all test cases included} \label{Tab:Summary} \\');
            fprintf(logid,'%s\n','\showrowcolors');
            fprintf(logid,'%s\n','\hline');
            fprintf(logid,'%s%s%s\n','\rowcolor{',Color.Titlebar,'} \STRUT \textbf{Validation case} & \textbf{Result file read} & \textbf{Result log files} & \textbf{Timing} \\ [1ex] \hline');
            fprintf(logid,'%s\n','\endfirsthead');
            fprintf(logid,'%%\n');
            fprintf(logid,'%s\n','\hiderowcolors');
            fprintf(logid,'%s\n','\multicolumn{5}{c}{{\STRUT \tablename\ \thetable{} -- continued from previous page}} \\ [1ex] \hline');
            fprintf(logid,'%s\n','\showrowcolors');
            fprintf(logid,'%s%s%s\n','\rowcolor{',Color.Titlebar,'} \STRUT \textbf{Validation case} & \textbf{Result file read} & \textbf{Result log files} & \textbf{Timing} \\ [1ex] \hline');
            fprintf(logid,'%s\n','\endhead');
            fprintf(logid,'%%\n');
            fprintf(logid,'%s\n','\hline');
            fprintf(logid,'%s\n','\hiderowcolors');
            fprintf(logid,'%s\n','\multicolumn{5}{r}{{\STRUT \tablename \thetable{} continues on next page}} \\');
            fprintf(logid,'%s\n','\showrowcolors');
            fprintf(logid,'%s\n','\endfoot');
            fprintf(logid,'%%\n');
            fprintf(logid,'%s\n','\endlastfoot');
        else
            fprintf(logid,'%% %s %s\n','Delft3D-QUICKPLOT validation report for case ',casename);
            casetype = sametype(casename);
            if ~isempty(casetype)
                fprintf(logid,'%s{%s} %s{Chap:%s}\n','\chapter',protected(casetype),'\label',makelabel(casetype));
            end
            fprintf(logid,'%s{%s} %s{Sec:%s}\n','\section',protected(casename),'\label',makelabel(casename));
            fprintf(logid,'\n');
        end
    otherwise
        fprintf(logid,'<html>\n<title>Delft3D-QUICKPLOT validation report</title>\n<body bgcolor=%s><font face=arial>\n',Color.Page);
        fprintf(logid,'<table bgcolor=%s><tr><td colspan=2 bgcolor=%s><b>Deltares validation report</b></td></tr>\n<tr><td>Program:</td><td>Delft3D-QUICKPLOT</td></tr>\n', ...
            Color.Table{1},Color.Titlebar);
        fprintf(logid,'<tr bgcolor=%s><td>Version:</td><td>%s%s</td></tr>\n<tr><td>Date:</td><td>%4.4i-%2.2i-%2.2i %2.2i:%2.2i:%02.0f</td></tr>\n',Color.Table{2},d3d_qp('version'),stalone,c);
        fprintf(logid,'<tr bgcolor=%s><td>MATLAB version:</td><td>%s</td></tr>\n',Color.Table{1},version);
        fprintf(logid,'<tr bgcolor=%s><td>Computer type:</td><td>%s</td></tr>\n',Color.Table{2},computer);
        if strcmp(casename,'MAIN')
            fprintf(logid,'<tr bgcolor=%s><td>Case:</td><td>%s</td></tr>\n',Color.Table{1},casename);
        end
        fprintf(logid,'</table><br>\n');
        if strcmp(casename,'MAIN')
            fprintf(logid,'<table bgcolor=%s>\n<tr bgcolor=%s><td><b>Validation case</b></td><td><b>Result<br>file read</b></td><td><b>Result<br>log files</b></td><td><b>View Log</b></td><td><b>Timing</b></td></tr>\n',Color.Table{1},Color.Titlebar);
        end
end
flush(logid)
t = datenum(c);


function write_table_error(logid,Color,err)
msg = stack2str(err.stack);
switch log_style
    case 'latex'
        message = protected(err.message);
        msg = protected(msg);
        fprintf(logid,'%s%s%s\n','\multicolumn{5}{|p{\textwidth-12pt}|}{\STRUT \textcolor{',Color.Failed,'}{');
        fprintf(logid,'Test bench execution failed unexpectedly. \\\\\n');
        fprintf(logid,'%s \\\\\n',message,msg{:});
        fprintf(logid,'%s\n','}}');
    otherwise
        fprintf(logid,'<tr><td colspan=5><font color=%s><b>Test bench execution failed unexpectedly.</b><br>\n',Color.Failed);
        fprintf(logid,'%s<br>',err.message,msg{:});
        fprintf(logid,'</font></td></tr>\n');
end


function write_table1_line(logid,bgcolor,casename,frcolor,frresult,lgcolor,lgresult,logname,dt2_str)
switch log_style
    case 'latex'
        if isempty(lgcolor)
            fprintf(logid,'\\STRUT \\nameref{Sec:%s} & \\textcolor{%s}{%s} &  & %s \\\\\n',makelabel(casename),frcolor,frresult,dt2_str);
        else
            fprintf(logid,'\\STRUT \\nameref{Sec:%s} & \\textcolor{%s}{%s} & \\textcolor{%s}{%s} & %s \\\\\n',makelabel(casename),frcolor,frresult,lgcolor,lgresult,dt2_str);
        end
    otherwise
        fprintf(logid,'<tr bgcolor=%s><td>%s</td>',bgcolor,casename);
        if isempty(lgcolor)
            fprintf(logid,'<td colspan=2><font color=%s><b>%s</b></font></td>',frcolor,frresult);
        else
            fprintf(logid,'<td><font color=%s><b>%s</b></font></td><td><font color=%s><b>%s</b></font></td>',frcolor,frresult,lgcolor,lgresult);
        end
        fprintf(logid,'<td><a href="%s/%s">Click</a></td><td>%s</td></tr>\n',casename,logname,dt2_str);
end


function write_table2_line(logid,bgcolor,Name,Read,Compare)
switch log_style
    case 'latex'
        % to set the color of a single row use '\rowcolor{...} '
        if ~ischar(Read)
            if isequal(Compare,1)
                fprintf(logid,'');
            elseif isequal(Compare,2)
                fprintf(logid,'\\\\\n');
            else
                fprintf(logid,'%s \\\\\n',Compare);
            end
        elseif ~ischar(Compare)
            fprintf(logid,'\\STRUT %s & %s & ',protected(Name),Read);
        else
            fprintf(logid,'\\STRUT %s & %s & %s \\\\\n',protected(Name),Read,Compare);
        end
    otherwise
        if ~ischar(Read)
            if isequal(Compare,1)
                fprintf(logid,'<td>');
            elseif isequal(Compare,2)
                fprintf(logid,'</td></tr>\n');
            else
                fprintf(logid,'<td>%s</td></tr>\n',Compare);
            end
        elseif ~ischar(Compare)
            fprintf(logid,'<tr bgcolor=%s><td>%s</td><td>%s</td>',bgcolor,Name,Read);
        else
            fprintf(logid,'<tr bgcolor=%s><td>%s</td><td>%s</td><td>%s</td></tr>\n',bgcolor,Name,Read,Compare);
        end
end


function write_rule(logid)
switch log_style
    case 'latex'
        fprintf(logid,'\n%% --- separator ---\n\n');
    otherwise
        fprintf(logid,'<hr>\n');
end


function write_log1(logid,message,varargin)
if nargin>2
    message = sprintf(message,varargin{:});
end
if ~iscell(message)
    message = {message};
end
switch log_style
    case 'latex'
        fprintf(logid,'%s',message{:});
    otherwise
        fprintf(logid,'%s',message{:});
end


function write_section(logid,message,varargin)
if nargin>2
    message = sprintf(message,varargin{:});
end
if ~iscell(message)
    message = {message};
end
switch log_style
    case 'latex'
        fprintf(logid,'\\section*{%s}\n\n',message{:});
    otherwise
        fprintf(logid,'%s<br>\n',message{:});
end


function write_log(logid,message,varargin)
if nargin>2
    message = sprintf(message,varargin{:});
end
if ~iscell(message)
    message = {message};
end
switch log_style
    case 'latex'
        fprintf(logid,'%s\\\\*\n',message{:});
    otherwise
        fprintf(logid,'%s<br>\n',message{:});
end


function str = color_write(str,color,bold)
if nargin<3
    bold = false;
end
switch log_style
    case 'latex'
        if bold
            str = sprintf('\textcolor{%s}{\textbf{%s}}',color,str);
        else
            str = sprintf('\textcolor{%s}{%s}',color,str);
        end
    otherwise
        if bold
            str = sprintf('<font color=%s><b>%s</b></font>',color,str);
        else
            str = sprintf('<font color=%s>%</font>',color,str);
        end
end

function include_figure(logid,checkf)
switch log_style
    case 'latex'
        fprintf(logid,'\\includegraphics*[width=50mm]{%s}\n\n',protect_filename(checkf));
    otherwise
        fprintf(logid,'<img src=''%s''><br>\n',checkf);
end


function include_diff_figures(logid,files,Color)
nfiles = length(files);
switch log_style
    case 'latex'
        fprintf(logid,'%s%s%s\n','\begin{tabular}{',repmat('l',1,nfiles),'}');
        fprintf(logid,'%s\n','\hiderowcolors');
        for i = 1:nfiles-1
            fprintf(logid,'%s%s%s','\textbf{',protected(files{i}),'} & ');
        end
        fprintf(logid,'%s%s%s\n','\textbf{',protected(files{end}),'} \\');
        %
        for i = 1:nfiles-1
            fprintf(logid,'%s%s%s','\includegraphics*[width=50mm]{',protect_filename(files{i}),'} & ');
        end
        fprintf(logid,'%s%s%s\n','\includegraphics*[width=50mm]{',protect_filename(files{end}),'} \\');
        fprintf(logid,'%s\n','\showrowcolors');
        fprintf(logid,'%s\n','\end{tabular}');
    otherwise
        fprintf(logid,'<table bgcolor=%s>\n',Color.Table{1});
        fprintf(logid,['<tr>' repmat(['<td width=300 bgcolor=',Color.Titlebar,'>%s</td>'],1,nfiles) '</tr>\n'],files{:});
        fprintf(logid,['<tr>',repmat('<td><img src=''%s''></td>',1,nfiles),'</tr>\n'],files{:});
        fprintf(logid,'</table>\n');
end


function write_table_rule(logid,Color)
switch log_style
    case 'latex'
        fprintf(logid,'\\hline\n');
    otherwise
        fprintf(logid,'<tr><td colspan=3 bgcolor=%s></td></tr>\n',Color.Titlebar);
end


function write_begin_table(logid,Color)
switch log_style
    case 'latex'
        fprintf(logid,'%s\n','\begin{longtable}{|l|l|l|}');
        %fprintf(logid,'%s\n','\hiderowcolors');
        %fprintf(logid,'%s\n','\caption{No caption} \\');
        %fprintf(logid,'%s\n','\showrowcolors');
        fprintf(logid,'%s\n','\hline');
        fprintf(logid,'%s%s%s\n','\rowcolor{',Color.Titlebar,'} \STRUT \textbf{Data field} & \textbf{Read} & \textbf{Compare} \\ [1ex] \hline');
        fprintf(logid,'%s\n','\endfirsthead');
        fprintf(logid,'%%\n');
        fprintf(logid,'%s\n','\hiderowcolors');
        fprintf(logid,'%s\n','\multicolumn{3}{c}{{\STRUT \tablename\ \thetable{} -- continued from previous page}} \\ [1ex] \hline');
        fprintf(logid,'%s\n','\showrowcolors');
        fprintf(logid,'%s%s%s\n','\rowcolor{',Color.Titlebar,'} \STRUT \textbf{Data field} & \textbf{Read} & \textbf{Compare} \\ [1ex] \hline');
        fprintf(logid,'%s\n','\endhead');
        fprintf(logid,'%%\n');
        fprintf(logid,'%s\n','\hline');
        fprintf(logid,'%s\n','\hiderowcolors');
        fprintf(logid,'%s\n','\multicolumn{3}{r}{{\STRUT \tablename \thetable{} continues on next page}} \\');
        fprintf(logid,'%s\n','\showrowcolors');
        fprintf(logid,'%s\n','\endfoot');
        fprintf(logid,'%%\n');
        fprintf(logid,'%s\n','\endlastfoot');
    otherwise
        fprintf(logid,'<table bgcolor=%s><tr><td bgcolor=%s><b>Data field</b></td><td bgcolor=%s><b>Read</b></td><td bgcolor=%s><b>Compare</b></td></tr>\n', ...
            Color.Table{1},Color.Titlebar,Color.Titlebar,Color.Titlebar);
end


function write_end_table(logid,landscape,emptyTable)
if nargin<3
    emptyTable = false;
end
switch log_style
    case 'latex'
        if emptyTable
            fprintf(logid,'%s\n','\STRUT & & \\');
        end
        fprintf(logid,'%s\n','[1ex] \hline');
        fprintf(logid,'%s\n','\end{longtable}');
        if landscape
            fprintf(logid,'%s\n','\end{landscape}');
        end
    otherwise
        fprintf(logid,'</table>\n');
end
fprintf(logid,'\n');


function write_includes(logid,includes)
switch log_style
    case 'latex'
        for i = 1:size(includes,1)
            if ~isempty(includes{i,1})
                fprintf(logid,'\\graphicspath{{"%s/"}}\n',includes{i,1});
                fprintf(logid,'\\input{"%s%s"}\n',includes{i,1},includes{i,2});
            end
        end
    otherwise
        % no includes needed
end

function [dt,dt_str] = write_footer(logid,casename,Color,t0,dt_old)
c = clock;
dt = (datenum(c)-t0)*86400;
dt_str = duration(dt);
if isnan(dt_old) || abs(dt-dt_old)<max(0.2,min(dt,dt_old)/30)
    color = Color.Font;
elseif dt>dt_old
    color = Color.Failed;
else
    color = Color.Success;
end
switch log_style
    case 'latex'
        if strcmp(casename,'MAIN')
            % main document
            fprintf(logid,'\n');
            fprintf(logid,'%% %s\n','\newpage');
            fprintf(logid,'%% %s\n','\pagestyle{empty}');
            fprintf(logid,'%% %s\n','\mbox{}');
            fprintf(logid,'%% %s\n','\includepdf[pages=2,offset=-72 -70]{pictures/delft3d-cover_quickplot.pdf} % links-rechts past precies');
            fprintf(logid,'%s\n','\end{document}');
        else
            write_section(logid,'Timing');
            if ~isequal(color,Color.Font)
                dt_str = ['\textcolor{',color,'}{',dt_str,'} previously: ',duration(dt_old)];
            end
            fprintf(logid,'Duration: %s\n',dt_str);
        end
    otherwise
        fprintf(logid,'<table bgcolor=%s><tr><td colspan=2 bgcolor=%s><b>End of validation report</b></td></tr>\n', ...
            Color.Table{1},Color.Titlebar);
        fprintf(logid,'<tr><td>Date:</td><td>%4.4i-%2.2i-%2.2i %2.2i:%2.2i:%02.0f</td></tr>\n',c);
        if ~isequal(color,Color.Font)
            dt_str = ['<font color=',color,'>',dt_str,'</font> previously: ',duration(dt_old)];
        end
        fprintf(logid,'<tr><td>Duration:</td><td>%s</td></tr>\n',dt_str);
        fprintf(logid,'</table><br>\n');
        fprintf(logid,'</font></body>');
end
flush(logid)


function s = duration(dt)
if dt>60
    mdt = floor(dt/60);
    sdt = dt-60*mdt;
    s = sprintf('%.1fs (%im %.1fs)',dt,mdt,sdt);
else
    s = sprintf('%.1fs',dt);
end


function X=localexist(file)
%X=exist(file);
X=fopen(file);
if X>0, fclose(X); end
X=X>0;


function flush(logid1)
fseek(logid1,0,-1);
fseek(logid1,0,1);


function ensure_directory(dirname)
if isempty(dir(dirname))
    [parent, thisdir, ext] = fileparts(dirname);
    thisdir = [thisdir ext];
    if ~isempty(parent)
        ensure_directory(parent);
        cd(parent)
    end
    c = computer;
    if strcmp(c(1:2),'PC')
        s = dos(['mkdir "',thisdir,'"']);
    else
        s = unix(['mkdir -p ',thisdir]);
    end
end


function localsave(filename,Data,saveops)
if isstandalone
    save(filename,'Data');
else
    save(filename,'Data',saveops{:});
end


function Data = localload(filename)
if isstandalone
    Data = load(filename);
else
    Data = load(filename,'-mat');
end


function xtype = log_style(type)
persistent current_type
if nargin>0
    current_type = type;
else
    xtype = current_type;
end

function type = sametype(casename)
persistent previous_type
sep = strfind(casename,' - ');
if isempty(sep)
    type = casename;
else
    type = casename(1:sep-1);
end
if isequal(type,previous_type)
    type = '';
else
    previous_type = type;
end