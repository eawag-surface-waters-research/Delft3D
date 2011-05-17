function varargout=arcgridfil(FI,domain,field,cmd,varargin)
%ARCGRIDFIL QP support for ARCGRID files.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXFIL(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXFIL(FI,[],'options',OptionsFigure,OptionsCommand, ...)
%
%   The DataFld can only be either an element of the DataProps structure.

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

%========================= GENERAL CODE =======================================

T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
    error('Not enough input arguments');
elseif nargin==2
    varargout={infile(FI,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(FI,cmd,varargin{:});
        case 'optionstransfer'
            varargout{1}=optionstransfer(FI,cmd);
        case 'domains'
            varargout={domains(FI)};
        case 'dimensions'
            varargout={dimensions(FI)};
        case 'locations'
            varargout={locations(FI)};
        case 'quantities'
            varargout={quantities(FI)};
        case 'data'
            [varargout{1:2}]=getdata(FI,cmd,varargin{:});
    end
    return
else
    Props=field;
end

cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(FI,Props)};
        return;
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'stations'
        varargout={readsts(FI,Props,0)};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

subf=getsubfields(FI,Props);
fidx=find(DimFlag);
if isempty(subf)
    % initialize and read indices ...
    idx={[] [] 0 0 0};
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    Props.SubFld=varargin{1};
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end

% select appropriate timestep ...
sz=getsize(FI,Props);
if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
    if isequal(idx{T_},0)
        idx{T_}=1:sz(T_);
    end
end

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
ind=cell(1,5);
ind{2}=1;
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) | isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error(sprintf('Only scalars or ranges allowed for index %i',i));
        end
    end
end

if max(idx{T_})>sz(T_) & ~(isequal(idx{T_},1) & sz(T_)==0)
    error(sprintf('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_)))
end

% read grid ...
x=[];
y=[];
z=[];
if XYRead

    %======================== SPECIFIC CODE =======================================
    if length(FI.CellSize)==1
        FI.CellSize = FI.CellSize([1 1]);
    end
    if XYRead & (DataInCell | Props.NVal==0)
        eidx=idx;
        eidx{M_}(end+1)=eidx{M_}(end)+1;
        eidx{N_}(end+1)=eidx{N_}(end)+1;
        x=transpose(FI.XCorner+((1:FI.NCols+1)-1)*FI.CellSize(1))*ones(1,FI.NRows+1);
        y=ones(FI.NCols+1,1)*(FI.YCorner+(FI.NRows+1)*FI.CellSize(2)-(1:FI.NRows+1)*FI.CellSize(2));
        x=x(eidx{[M_ N_]});
        y=y(eidx{[M_ N_]});
    else
        x=transpose(FI.XCorner+((1:FI.NCols)-0.5)*FI.CellSize(1))*ones(1,FI.NRows);
        y=ones(FI.NCols,1)*(FI.YCorner+(FI.NRows+0.5)*FI.CellSize(2)-(1:FI.NRows)*FI.CellSize(2));
        x=x(idx{[M_ N_]});
        y=y(idx{[M_ N_]});
    end
    if DimFlag(K_)
        z=ones(size(x));
    end
    %========================= GENERAL CODE =======================================
end

if DataRead & Props.NVal>0
    if strcmp(lower(FI.Extension),'amuv')
        nM = length(idx{M_});
        nN = length(idx{N_});
        val1 = zeros(length(idx{T_}),nM,nN);
        for ti = 1:length(idx{T_})
            t = idx{T_}(ti);
            [val1full,val2full]=arcgrid('read',FI,t);
            val1(ti,:,:)=reshape(val1full(idx{[M_ N_]}),[1 nM nN]);
            val2(ti,:,:)=reshape(val2full(idx{[M_ N_]}),[1 nM nN]);
        end
        if length(idx{T_})==1
            val1 = reshape(val1,nM,nN);
            val2 = reshape(val2,nM,nN);
        end
    else
        if isfield(Props,'SubFld')
            val1=arcgrid('read',FI,Props.SubFld);
            val1=val1(idx{[M_ N_]});
        elseif sz(T_)>0
            nM = length(idx{M_});
            nN = length(idx{N_});
            val1 = zeros(length(idx{T_}),nM,nN);
            for ti = 1:length(idx{T_})
                t = idx{T_}(ti);
                val1full=arcgrid('read',FI,t);
                val1(ti,:,:)=reshape(val1full(idx{[M_ N_]}),[1 nM nN]);
            end
            if length(idx{T_})==1
                val1 = reshape(val1,nM,nN);
            end
        else
            val1=arcgrid('read',FI);
            val1=val1(idx{[M_ N_]});
        end
        val2=[];
    end

    switch Props.Sign
        case -1
            val1=-val1;
    end
else
    Props.NVal=0;
end

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
    if DimFlag(K_)
        Ans.Z=z;
    end
end
if Props.NVal==0
elseif isempty(val2)
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end

if Props.DimFlag(T_)
    % read time ...
    Ans.Time=readtim(FI,Props,idx{T_});
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'               'Units'  'DimFlag' 'DataInCell' 'NVal'   'Sign'   };
DataProps={'grid'               ''        [0 0 1 1 0]  0         0         0
    '-------'                   ''        [0 0 0 0 0]  0         0         0
    'data'                      ''        [3 0 1 1 0]  1         1         1
    'negated data'              ''        [3 0 1 1 0]  1         1        -1      };
%------------------------------------------------------------------------------
Out=cell2struct(DataProps,PropNames,2);
%------------------------------------------------------------------------------
i2=length(Out);
i1=i2-1;
switch lower(FI.Extension)
    case 'adp'
        Out(i2).Name='terrain elevation';
        Out(i2).Units='m';
        Out(i1,:)=[];
    case 'aht'
        Out(i1).Name='terrain elevation';
        Out(i1).Units='m';
        Out(i2,:)=[];
    case 'amh'
        Out(i1).Name='water depth';
        Out(i1).Units='m';
        Out(i2,:)=[];
    case 'amz'
        Out(i1).Name='water level';
        Out(i1).Units='m';
        Out(i2,:)=[];
    case 'amuv'
        Out(i1).Name='velocity';
        Out(i1).Units='m/s';
        Out(i1).NVal=2;
        Out(i2,:)=[];
    case 'amc'
        Out(i1).Name='velocity magnitude';
        Out(i1).Units='m/s';
        Out(i2,:)=[];
    case 'amu'
        Out(i1).Name='u velocity';
        Out(i1).Units='m/s';
        Out(i2,:)=[];
    case 'amv'
        Out(i1).Name='v velocity';
        Out(i1).Units='m/s';
        Out(i2,:)=[];
    case 'amp'
        Out(i1).Name='pressure';
        Out(i1).Units='N/m^2';
        Out(i2,:)=[];
    case 'afb'
        Out(i1).Name='bed friction coefficient';
        Out(i2,:)=[];
    case 'afw'
        Out(i1).Name='wall friction coefficient';
        Out(i2,:)=[];
    case 'acx'
        Out(i1).Name='maximum velocity magnitude';
        Out(i1).Units='m/s';
        Out(i2,:)=[];
    case 'ahx'
        Out(i1).Name='maximum water depth';
        Out(i1).Units='m';
        Out(i2,:)=[];
    otherwise
        [p,f]=fileparts(FI.FileBase);
        if length(f)>2 && isequal(f(1:2),'dm')
            subtype=char(sscanf(f,'dm%*d%s',1)');
            switch subtype
                case 'c'
                    Out(i1).Name='velocity magnitude';
                    Out(i1).Units='m/s';
                    Out(i2,:)=[];
                case 'd'
                    Out(i1).Name='water depth';
                    Out(i1).Units='m';
                    Out(i2,:)=[];
                case 'emc'
                    Out(i1).Name='time of class drying';
                    Out(i1).Units='h';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'h'
                    Out(i1).Name='water level';
                    Out(i1).Units='m';
                    Out(i2,:)=[];
                case 'maxc'
                    Out(i1).Name='maximum velocity magnitude';
                    Out(i1).Units='m/s';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'maxd'
                    Out(i1).Name='maximum water depth';
                    Out(i1).Units='m';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'maxh'
                    Out(i1).Name='maximum water level';
                    Out(i1).Units='m';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'maxr'
                    Out(i1).Name='maximum water level change rate';
                    Out(i1).Units='m/s';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'negdp'
                    Out(i1).Name='number of time step reductions due to negative depth';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'tem'
                    Out(i1).Name='time of drying';
                    Out(i1).Units='h';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'tmaxc'
                    Out(i1).Name='time of maximum velocity magnitude';
                    Out(i1).Units='h';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'tmaxd'
                    Out(i1).Name='time of maximum water depth';
                    Out(i1).Units='h';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'tmaxh'
                    Out(i1).Name='time of maximum water level';
                    Out(i1).Units='h';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'tsu'
                    Out(i1).Name='number of time step reductions due to u courant';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'tsv'
                    Out(i1).Name='number of time step reductions due to v courant';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'twt'
                    Out(i1).Name='time of wetting';
                    Out(i1).Units='h';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
                case 'u'
                    Out(i1).Name='u velocity';
                    Out(i1).Units='m/s';
                    Out(i2,:)=[];
                case 'v'
                    Out(i1).Name='v velocity';
                    Out(i1).Units='m/s';
                    Out(i2,:)=[];
                case 'wtc'
                    Out(i1).Name='time of class wetting';
                    Out(i1).Units='h';
                    Out(i2,:)=[];
                    Out(i1).DimFlag(1) = 0;
            end
        end
end

%--- set UseGrid options ...
[Out(:).UseGrid]=deal(1);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
T_=1; ST_=2; M_=3; N_=4; K_=5;
if length(FI.Times)>1 & Props.DimFlag(T_)==0 & Props.NVal==1
    subf=cell(length(FI.Times),1);
    for i=1:length(FI.Times)
        subf{i}=sprintf('class %i',i);
    end
else
    subf={};
end
if nargin>2 & f~=0
    subf=subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(M_) & Props.DimFlag(N_)
    sz([M_ N_])=[FI.NCols FI.NRows];
end;
if Props.DimFlag(K_)
    sz(K_)=1;
end
if Props.DimFlag(T_)
    if ~isempty(FI.Times)
        sz(T_)=length(FI.Times);
    else
        sz(T_)=0;
    end
end
if Props.DimFlag(ST_)
    sz(ST_)=0;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if ~isempty(FI.Times)
    T=FI.Times/24;
    if ~isequal(t,0)
        T=T(t);
    end
else
    T=0;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)

%======================== SPECIFIC CODE =======================================
S={};
% -----------------------------------------------------------------------------
