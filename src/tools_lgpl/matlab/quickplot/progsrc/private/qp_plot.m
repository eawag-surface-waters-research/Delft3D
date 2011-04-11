function [hNewVec,Error,FileInfo]=qp_plot(PlotState)
%QP_PLOT Plot function of QuickPlot.

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
%   http://www.delftsoftware.com
%   $HeadURL$
%   $Id$

T_=1; ST_=2; M_=3; N_=4; K_=5;

hNewVec=0;
Error=1;
FileInfo=PlotState.FI;

Domain=PlotState.Domain;
Props=PlotState.Props;
SubField=PlotState.SubField;
Selected=PlotState.Selected;
Parent=PlotState.Parent;
hOld=PlotState.Handles;
stats=PlotState.Stations;
Ops=PlotState.Ops;

if iscell(hOld)
    hOldVec=cat(1,hOld{:});
else
    hOldVec=hOld;
    hOld={hOld};
end
DimFlag=Props.DimFlag;

Ops=qp_state_version(Ops);
if isequal(Ops.horizontalalignment,'centre')
    Ops.horizontalalignment='center';
end

hNewTag='';
lParents=get(hOldVec(ishandle(hOldVec)),'Parent');
if iscell(lParents)
    lParents=unique([lParents{:}]);
end
for lP=1:length(lParents)
    Parent=lParents(lP);
    SortObjs=1;
    Pchild=allchild(Parent);
    if isempty(hOldVec) || ~ishandle(hOldVec(1))
        PchildBefore{lP}=[];
        PchildAfter{lP}=Pchild;
    else
        hNewTag=get(hOldVec(1),'tag');
        IdxObj=find(Pchild==hOldVec(1));
        if isempty(IdxObj)
            SortObjs=0;
            PchildBefore{lP}=[];
            PchildAfter{lP}=[];
        else
            PchildBefore{lP}=Pchild(1:IdxObj); PchildBefore{lP}=PchildBefore{lP}(~ismember(PchildBefore{lP},hOldVec));
            PchildAfter{lP}=Pchild(IdxObj:end); PchildAfter{lP}=PchildAfter{lP}(~ismember(PchildAfter{lP},hOldVec));
        end
    end
    %  else
    %    if ishandle(hOldVec(1))
    %      hNewTag=get(hOldVec(1),'tag');
    %    end
    %    SortObjs=0;
    %    Pchild=[];
    %    PchildBefore=[];
    %    PchildAfter=[];
    %  end
end
Thresholds=[]; % Thresholds is predefined to make sure that Thresholds always exists when its value is checked at the end of this routine

SubSelected=Selected;
SubSelected(~DimFlag)=[];
FT=FileInfo.FileType;
if isequal(Ops.presentationtype,'vector (split m,n)')
    Props.MNK=-1;
elseif isfield(Ops,'MNK') && Ops.MNK
    Props.MNK=1.5;
end

if Props.NVal==-1
    data=[];
else
    if isfield(Ops,'extend2edge') && Ops.extend2edge
        [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'griddefdata',SubField{:},SubSelected{:});
    else
        switch Ops.presentationtype
            case {'patches','patches with lines','patch centred vector'}
                [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'gridcelldata',SubField{:},SubSelected{:});
            otherwise
                [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'griddata',SubField{:},SubSelected{:});
        end
    end
    if isempty(data)
        ui_message('error',sprintf('Did not get any data from %s file.',FT));
        return
    elseif ~Chk
        ui_message('error','Error retrieving data from file.');
        return
    end
end

PName=Props.Name;
Units='';
if ~isempty(data)
    Units=data(1).Units;
end
if isequal(Ops.units,'**Hide**')
    Units='';
elseif ~isempty(Ops.units) && ~isempty(Units)
    dataX=qp_unitconversion(Units,Ops.units,data);
    if ~ischar(dataX)
        data=dataX;
        dataX=[];
        Units=data(1).Units;
    end
end

for i=5:-1:1
    multiple(i) = (length(Selected{i})>1) | isequal(Selected{i},0);
end

di=1+~multiple(T_);
for i=5:-1:3
    if ~multiple(i) && DimFlag(i), % deal with DataInCell extended grid
       for d=1:length(data)
          geom='';
          if isfield(data,'Geom') && ~isempty(data(d).Geom)
             geom=data(d).Geom;
          elseif isfield(Props,'Geom')
             geom=Props.Geom;
          end
          if ~strcmp(geom,'POLYL')
             if isfield(data,'X') && size(data(d).X,i-di)>1
                data(d).X=mean(data(d).X,i-di);
             end
             if isfield(data,'Y') && size(data(d).Y,i-di)>1
                data(d).Y=mean(data(d).Y,i-di);
             end
             if isfield(data,'Z') && size(data(d).Z,i-di)>1
                data(d).Z=mean(data(d).Z,i-di);
             end
          end
       end
    end
end

FirstFrame=isempty(hOldVec);
s=[];

if isfield(data,'TRI')
    switch lower(Ops.thinningmode)
        case 'none'
            data.X=data.XYZ(:,:,:,1);
            data.Y=data.XYZ(:,:,:,2);
            if size(data.XYZ,4)>2
                data.Z=data.XYZ(:,:,:,3);
            end
        case 'uniform'
            Fld=logical(zeros(1,size(data.XYZ,2)));
            Fld(1:Ops.thinningfactors(1):end)=1;
            data.X=data.XYZ(:,Fld,:,1);
            data.Y=data.XYZ(:,Fld,:,2);
            if size(data.XYZ,4)>2
                data.Z=data.XYZ(:,Fld,:,3);
            end
            if isfield(data,'XComp')
                data.XComp=data.XComp(:,Fld,:);
            end
            if isfield(data,'YComp')
                data.YComp=data.YComp(:,Fld,:);
            end
            if isfield(data,'ZComp')
                data.ZComp=data.ZComp(:,Fld,:);
            end
            if isfield(data,'Val')
                data.Val  =data.Val(:,Fld,:);
            end
        case 'distance'
            I=all(~isnan(data.XYZ(1,:,1,:)),4);
            II=find(I);
            Ind=reducepoints(Ops.thinningdistance,data.XYZ(1,I,1,1),data.XYZ(1,I,1,2));
            I=logical(zeros(size(I))); I(II(Ind))=1;
            data.X=data.XYZ(:,I,:,1);
            data.Y=data.XYZ(:,I,:,2);
            if size(data.XYZ,4)>2
                data.Z=data.XYZ(:,I,:,3);
            end
            if isfield(data,'XComp')
                data.XComp=data.XComp(I);
            end
            if isfield(data,'YComp')
                data.YComp=data.YComp(I);
            end
            if isfield(data,'ZComp')
                data.ZComp=data.ZComp(I);
            end
            if isfield(data,'Val')
                data.Val  =data.Val(I);
            end
    end
else
    switch lower(Ops.thinningmode)
        case 'none'
        case 'uniform'
            if isfield(data,'Val')
                Fld=logical(zeros(size(data.Val)));
            else
                Fld=logical(zeros(size(data.XComp)));
            end
            Fld(1:Ops.thinningfactors(1):end,1:Ops.thinningfactors(2):end,1:Ops.thinningfactors(3):end)=1;
            sz(3)=sum(Fld(1,1,:));
            sz(2)=sum(Fld(1,:,1));
            sz(1)=sum(Fld(:,1,1));
            if isfield(data,'X')
                data.X=reshape(data.X(Fld),sz);
            end
            if isfield(data,'Y')
                data.Y=reshape(data.Y(Fld),sz);
            end
            if isfield(data,'Z')
                data.Z=reshape(data.Z(Fld),sz);
            end
            if isfield(data,'XComp')
                data.XComp=reshape(data.XComp(Fld),sz);
            end
            if isfield(data,'YComp')
                data.YComp=reshape(data.YComp(Fld),sz);
            end
            if isfield(data,'ZComp')
                data.ZComp=reshape(data.ZComp(Fld),sz);
            end
            if isfield(data,'Val')
                data.Val  =reshape(data.Val(Fld)  ,sz);
            end
        case 'distance'
            if isfield(data,'Y') && isfield(data,'Z')
                I=~isnan(data.X) & ~isnan(data.Y) & ~isnan(data.Z);
                II=find(I);
                Ind=reducepoints(Ops.thinningdistance,data.X(I),data.Y(I),data.Z(I));
            elseif isfield(data,'Y')
                I=~isnan(data.X) & ~isnan(data.Y);
                II=find(I);
                Ind=reducepoints(Ops.thinningdistance,data.X(I),data.Y(I));
            elseif isfield(data,'Z')
                I=~isnan(data.X) & ~isnan(data.Z);
                II=find(I);
                Ind=reducepoints(Ops.thinningdistance,data.X(I),data.Z(I));
            else
                I=~isnan(data.X);
                II=find(I);
                Ind=reducepoints(Ops.thinningdistance,data.X(I));
            end
            I=logical(zeros(size(I))); I(II(Ind))=1;
            if (multiple(M_) || multiple(N_)) && multiple(K_) && isfield(data,'Y')
                s=pathdistance(data.X(:,:,1),data.Y(:,:,1));
                s=reshape(repmat(s,[1 1 size(data.X,3)]),size(data.X));
                s=s(I);
            end
            if isfield(data,'X')
                data.X=data.X(I);
            end
            if isfield(data,'Y')
                data.Y=data.Y(I);
            end
            if isfield(data,'Z')
                data.Z=data.Z(I);
            end
            if isfield(data,'XComp')
                data.XComp=data.XComp(I);
            end
            if isfield(data,'YComp')
                data.YComp=data.YComp(I);
            end
            if isfield(data,'ZComp')
                data.ZComp=data.ZComp(I);
            end
            if isfield(data,'Val')
                data.Val  =data.Val(I);
            end
    end
end

vpt='';
[NVal,NValStr]=convertnval(Props.NVal);
if NVal==0.6 || NVal==0.9
    NVal=0.5;
end

VectorPlot=0;
if isfield(data,'XComp')
    [data,scalar]=computecomponent(data,Ops);
    if ~isempty(strfind(Ops.vectorcomponent,'vector'))
        % no text added for vector and patch centred vector
        if strcmp(strtok(Ops.vectorcolour),'angle')
            PName=[PName ', ' Ops.vectorcolour];
            Units='';
        elseif ~isempty(Ops.vectorcolour)
            PName=[PName ', ' Ops.vectorcolour];
        elseif Ops.spatial>1
            Units='';
        end
    elseif strcmp(strtok(Ops.vectorcomponent),'angle')
        PName=[PName ', ' Ops.vectorcomponent];
        Units='';
    else
        PName=[PName ', ' Ops.vectorcomponent];
    end
    if scalar
        NVal=1;
    else
        VectorPlot=1;
    end
end

minx=inf;
miny=inf;
minz=inf;
minv=inf;
maxx=-inf;
maxy=-inf;
maxz=-inf;
maxv=-inf;
for d=1:length(data)
    if isfield(data,'X') % time series may not have an X co-ordinate !
        minx=min(minx,min(data(d).X(:)));
        maxx=max(maxx,max(data(d).X(:)));
    end
    if isfield(data,'Y')
        miny=min(miny,min(data(d).Y(:)));
        maxy=max(maxy,max(data(d).Y(:)));
    end
    if isfield(data,'Z')
        minz=min(minz,min(data(d).Z(:)));
        maxz=max(maxz,max(data(d).Z(:)));
    end
end
npnt=0;
if isfield(data,'Val') && ~iscell(data(1).Val) && ~VectorPlot
    for d=1:length(data)
        if isfield(data,'X')
            szX=size(data(d).X);
        elseif isfield(data,'Y')
            szX=size(data(d).Y);
        elseif isfield(data,'Z')
            szX=size(data(d).Z);
        else
            szX=-1;
        end
        if  isequal(szX,size(data(d).Val))
            if isfield(data,'X')
                I=isnan(data(d).X);
                data(d).Val(I)=NaN;
                %data(d).X(I)=maxx;
            end
            if isfield(data,'Y')
                I=isnan(data(d).Y);
                data(d).Val(I)=NaN;
                %data(d).Y(I)=maxy;
            end
            if isfield(data,'Z')
                I=isnan(data(d).Z);
                data(d).Val(I)=NaN;
                %data(d).Z(I)=maxz;
            end
        end
        npnt=npnt+sum(~isnan(data(d).Val(:)));
        maxv=max(maxv,max(data(d).Val(:)));
        minv=min(minv,min(data(d).Val(:)));
    end
elseif isfield(data,'XComp')
    for d=1:length(data)
        I = data(d).XComp(:)==0;
        if isfield(data,'YComp')
            I = I & data(d).YComp(:)==0;
        end
        if isfield(data,'ZComp')
            I = I & data(d).ZComp(:)==0;
        end
        I = I | isnan(data(d).XComp(:));
        npnt=npnt+sum(~I);
    end
    npnt=max(npnt,1);
end

switch Ops.vectorscalingmode
    case ''
        quivopt={};
    case 'automatic'
        quivopt={'automatic'};
    case 'manual'
        for d=1:length(data)
            if isfield(data,'XComp')
                data(d).XComp=Ops.vectorscale*data(d).XComp;
            end
            if isfield(data,'YComp')
                data(d).YComp=Ops.vectorscale*data(d).YComp;
            end
            if isfield(data,'ZComp')
                data(d).ZComp=Ops.vectorscale*data(d).ZComp;
            end
        end
        quivopt={0};
    case {'manual normalised','automatic normalised'}
        for d=1:length(data)
            if strcmp(vpt,'magnitude')
                VecMag=data(d).Val;
            else
                VecMag=data(d).XComp.^2;
                if isfield(data,'YComp');
                    VecMag=VecMag+data(d).YComp.^2;
                end
                if isfield(data,'ZComp');
                    VecMag=VecMag+data(d).ZComp.^2;
                end
                VecMag=sqrt(VecMag);
            end
            %VecMag(VecMag<1e-3)=NaN;
            VecMag(VecMag==0)=1;
            if isfield(data,'XComp')
                data(d).XComp=Ops.vectorscale*data(d).XComp./VecMag;
            end
            if isfield(data,'YComp')
                data(d).YComp=Ops.vectorscale*data(d).YComp./VecMag;
            end
            if isfield(data,'ZComp')
                data(d).ZComp=Ops.vectorscale*data(d).ZComp./VecMag;
            end
        end
        if strcmp(Ops.vectorscalingmode,'manual normalised')
            quivopt={0};
        else
            quivopt={'automatic'};
        end
end

if isequal(quivopt,{'automatic'})
    del2=0;
    if isfield(data,'X')
        del2 = del2 + (maxx-minx).^2/npnt;
    end
    if isfield(data,'Y')
        del2 = del2 + (maxy-miny).^2/npnt;
    end
    if isfield(data,'Z') && isfield(data,'ZComp')
        del2 = del2 + (maxz-minz).^2/npnt;
    end
    maxlen=0;
    for d=1:length(data)
        V=0;
        if isfield(data,'XComp')
            V=V+data(d).XComp.^2;
        end
        if isfield(data,'YComp')
            V=V+data(d).YComp.^2;
        end
        if isfield(data,'ZComp') && isfield(data,'Z')
            V=V+data(d).ZComp.^2;
        end
        maxlen=max(maxlen,max(V(:)));
    end
    if del2~=0
        maxlen=sqrt(maxlen/del2);
        if maxlen>0
            autoscale = 0.9 / maxlen;
        else
            autoscale = 0.9;
        end
    else
        autoscale = 0.9;
    end
    %
    if 1
        %
        % apply "automatic" scaling
        %
        for d=1:length(data)
            if isfield(data,'XComp')
                data(d).XComp=autoscale*data(d).XComp;
            end
            if isfield(data,'YComp')
                data(d).YComp=autoscale*data(d).YComp;
            end
            if isfield(data,'ZComp')
                data(d).ZComp=autoscale*data(d).ZComp;
            end
        end
        %
        quivopt={0};
    else
        quivopt={};
    end
end

stn='';
if any(cellfun('isclass',Selected,'cell'))
    stn='';
elseif length(Selected{ST_})>1 || isequal(Selected{ST_},0)
    stn='<multiple>';
elseif ~isempty(stats)
    if iscell(stats)
        stn=stats{Selected{ST_}};
    else
        stn=deblank(stats(Selected{ST_},:));
    end
elseif DimFlag(ST_)
    [Chk,stn]=qp_getdata(FileInfo,Domain,Props,'stations');
    if iscell(stn)
        stn=stn{Selected{ST_}};
    else
        stn=stn(Selected{ST_},:);
    end
elseif any(multiple([M_ N_ K_]))
    stn='';
elseif DimFlag(M_) && DimFlag(N_) && DimFlag(K_)
    stn=sprintf('point (%i,%i,%i)',Selected{M_},Selected{N_},Selected{K_});
elseif DimFlag(M_) && DimFlag(K_)
    stn=sprintf('point (%i,%i)',Selected{M_},Selected{K_});
elseif DimFlag(M_) && DimFlag(N_)
    stn=sprintf('point (%i,%i)',Selected{M_},Selected{N_});
elseif DimFlag(M_)
    stn=sprintf('point %i',Selected{M_});
end
stn=strrep(stn,'\','\\');
stn=strrep(stn,'_','\_');

if ~isempty(SubField)
    [Chk,subfs]=qp_getdata(FileInfo,Domain,Props,'subfields',SubField{1});
    if Chk && ~isempty(subfs)
        PName=cat(2,PName,': ',subfs{1});
    end
end
PName=strrep(PName,'\','\\');
PName=strrep(PName,'{','\{');
PName=strrep(PName,'}','\}');
PName=strrep(PName,'_','\_');
if ~isempty(Units)
    PName=[PName ' (' Units ')'];
end

TStr='';
if isfield(data,'Time') && length(data(1).Time)==1
    switch DimFlag(T_)
        case {1,2} % day-month-year h:m:s
            % 1: discrete, 2: continuous
            TStr=datestr(data(1).Time,0);
        case {3,4} % day h:m:s
            % 3: discrete, 4: continuous
            TStr=cat(2,'day ',num2str(floor(data(1).Time)),' ',datestr(rem(data(1).Time,1),13));
        case {5,6} % i
            % 5: discrete, 6: continuous
            TStr=num2str(data(1).Time);
        case {7,8} % seconds
            % 7: discrete, 8: continuous
            TStr=cat(2,num2str(data(1).Time*24*3600),' s');
    end
end

clippingspatial = 0;
for clipi=1:3
    switch clipi
        case 1
            % clipping based on components gives all kinds of practical problems
            % (e.g. rotated components), so not implemented.
            fld={'Val','XDamVal','YDamVal'};
            clp='clippingvalues';
        case 2
            fld={'X'};
            clp='xclipping';
        case 3
            fld={'Y'};
            clp='yclipping';
    end
    if isfield(Ops,clp)
        clippingvals = getfield(Ops,clp);
    else
        clippingvals = [];
    end
    %
    if ~isempty(clippingvals)
        if clipi>1
            clippingspatial = 1;
        end
        for f=1:length(fld)
            fldf = fld{f};
            if isfield(data,fldf)
                for d=1:length(data)
                    val=getfield(data,{d},fldf);
                    data=setfield(data,{d},fldf,[]);
                    if isnumeric(clippingvals)
                        val(logical(ismember(val,clippingvals)))=NaN;
                    else
                        val=realset(clippingvals,val);
                    end
                    data=setfield(data,{d},fldf,val);
                end
            end
        end
    end
    val=[];
end
if isfield(data,'XYZ') && clippingspatial
    for d = 1:length(data)
       val = data(d).XYZ;
       szVal = size(val);
       val = reshape(val,prod(szVal(1:end-1)),szVal(end));
       for clipi = 2:3
           switch clipi
               case 2
                   dim = 1;
                   clippingvals = Ops.xclipping;
               case 3
                   dim = 2;
                   clippingvals = Ops.yclipping;
           end
           if isempty(clippingvals)
               % nothing
           elseif isnumeric(clippingvals)
               val(logical(ismember(val(:,dim),clippingvals)),dim)=NaN;
           else
               val(:,dim)=realset(clippingvals,val(:,dim));
           end
       end
       val = reshape(val,szVal);
       data(d).XYZ = val;
    end
    val=[];
end

LocLabelClass=0;
LocStartClass=0;
if strcmp(Ops.presentationtype,'coloured contour lines')
    LocLabelClass=1;
    LocStartClass=1;
end

if ~isempty(Parent) && ishandle(Parent) && strcmp(get(Parent,'type'),'axes')
    pfig=get(Parent,'parent');
    set(0,'currentfigure',pfig)
    set(pfig,'currentaxes',Parent)
end

if isfield(Ops,'linestyle')
    Ops.LineParams={'color',Ops.colour, ...
        'linewidth',Ops.linewidth, ...
        'linestyle',Ops.linestyle, ...
        'marker',Ops.marker, ...
        'markeredgecolor',Ops.markercolour, ...
        'markerfacecolor',Ops.markerfillcolour};
end
Ops.FontParams={'color',Ops.colour, ...
    'fontunits','points', ...
    'fontsize',Ops.fontsize, ...
    'horizontalalignment',Ops.horizontalalignment, ...
    'verticalalignment',Ops.verticalalignment};
if matlabversionnumber>=6.05
    if strcmp(Ops.textboxfacecolour,'none')
        TextBoxParams={'edgecolor','none','backgroundcolor','none'};
    else
        TextBoxParams={'edgecolor',Ops.colour,'backgroundcolor',Ops.textboxfacecolour};
    end
    Ops.FontParams=cat(2,Ops.FontParams,TextBoxParams);
end

%
% If horizontal units is degrees, change to longitude and latitude plot
% type.
%
if isfield(data,'XUnits') && strcmp(data(1).XUnits,'deg')
    switch Ops.axestype
        case 'X-Y'
            Ops.axestype='Lon-Lat';
        case 'X-Y-Z'
            Ops.axestype='Lon-Lat-Z';
        case 'X-Y-Val'
            Ops.axestype='Lon-Lat-Val';
    end
end
%
% Define basicaxestype as the axestype without the unit of the Z/Value
% field.
%
axestype=Ops.axestype;
unitsloc=strfind(axestype,' [');
for i=length(unitsloc):-1:1
    unitsclose=strfind(axestype(unitsloc(i):end),']');
    if ~isempty(unitsclose)
        axestype(:,unitsloc(i)+(0:max(unitsclose)-1))=[];
    end
end
basicaxestype=axestype;
%
% If it the plot contains a Z co-ordinate.
%
if ~isempty(strfind(basicaxestype,'Z'))
    %
    % If the elevation unit has not yet been specified, do so now.
    %
    if isempty(strfind(Ops.axestype,'Z ['))
        %
        % Get it either from the ZUnit field or assume it to be metres.
        %
        if isfield(data,'ZUnit') && ~isempty(data(1).ZUnit)
            Zunit=cat(2,'[',data(1).ZUnit,']');
        else
            Zunit='[m]';
        end
        Ops.axestype=cat(2,Ops.axestype,' ',Zunit);
    end
end

ChangeCLim=1;

diststr = 'x coordinate';
if isfield(Ops,'plotcoordinate') && ~isempty(Ops.plotcoordinate)
    switch Ops.plotcoordinate
        case {'path distance','reverse path distance'}
            if multiple(M_) && ~multiple(N_)
                if isempty(Selected{N_})
                    diststr = 'distance';
                else
                    name=sprintf('n=%i',Selected{N_});
                    diststr = sprintf('distance along cross-section %s',name);
                end
            elseif multiple(N_) && ~multiple(M_)
                if isempty(Selected{M_})
                    diststr = 'distance';
                else
                    name=sprintf('m=%i',Selected{M_});
                    diststr = sprintf('distance along cross-section %s',name);
                end
            end
        case 'x coordinate'
            diststr = 'x coordinate';
            if isfield(data,'XUnits') && isequal(data(1).XUnits,'deg')
                diststr = 'longitude';
            end
        case 'y coordinate'
            diststr = 'y coordinate';
            if isfield(data,'XUnits') && isequal(data(1).XUnits,'deg')
                diststr = 'latitude';
            end
    end
end

if NVal==4
    switch Ops.presentationtype
        case {'markers','tracks'}
            NVal = 0;
    end
end

%==========================================================================
% Begin of actual plotting
%==========================================================================
if NVal==-1
    [Chk,hNewVec,FileInfo]=qp_getdata(FileInfo,Domain,Props,'plot',Parent,Ops,SubField{:},SubSelected{:});
    hNew{1}=hNewVec;
    hObsolete=setdiff(hOld{1},hNew{1});
    delete(hObsolete(ishandle(hObsolete)));
    if ~isempty(Ops.axestype)
        setaxesprops(Parent,Ops.axestype)
    else
        set(Parent,'layer','top')
        %   elseif ~isappdata(Parent,'AxesType')
        %      setaxesprops(Parent,'<blocking>')
    end
else
    Param.ChangeCLim=1;
    Param.NVal=NVal;
    Param.multiple=multiple;
    Param.FirstFrame=FirstFrame;
    Param.PName=PName;
    Param.TStr=TStr;
    Param.Selected=Selected;
    Param.quivopt=quivopt;
    Param.stats=stats;
    Param.LocStartClass=LocStartClass;
    Param.stn=stn;
    Param.s=s;
    Param.compat7={};
    if matlabversionnumber>=7
        Param.compat7={'v6'};
    end

    for d=1:length(data)
        do=min(length(hOld),d);
        plotargs={hOld{do},Parent,Param,data(d),Ops,Props};
        geom='';
        if isfield(data,'Geom') && ~isempty(data(d).Geom)
            geom=data(d).Geom;
        elseif isfield(Props,'Geom')
            geom=Props.Geom;
        end
        switch geom
            case 'SEG'
                [hNew{d},Thresholds,Param]=qp_plot_seg(plotargs{:});
            case 'PNT'
                [hNew{d},Thresholds,Param]=qp_plot_pnt(plotargs{:});
            case 'POLYL'
                [hNew{d},Thresholds,Param]=qp_plot_polyl(plotargs{:});
            otherwise
                [hNew{d},Thresholds,Param]=qp_plot_default(plotargs{:});
        end
    end

    ChangeCLim=Param.ChangeCLim;
    hNewVec=cat(1,hNew{:});
end

if ~isempty(basicaxestype)
    axestype = strrep(strtok(basicaxestype),'-',' ');
    axestype = multiline(axestype,' ','cell');
    if isempty(axestype)
        dimension1 = '';
    elseif isequal(axestype{1},'Val')
        dimension1 = PName;
    else
        dimension1 = diststr;%'distance';
        if isfield(Ops,'plotcoordinate') && strcmp(Ops.plotcoordinate,'y coordinate')
            if isfield(Props,'NName') && ~isempty(Props.NName)
                dimension1 = Props.NName;
            end
            if isfield(data,'YUnits') && ~isempty(data(1).YUnits)
                dimension1 = [dimension1 ' (' data(1).YUnits ')'];
            end
        else
            if isfield(Props,'MName') && ~isempty(Props.MName)
                dimension1 = Props.MName;
            end
            if isfield(data,'XUnits') && ~isempty(data(1).XUnits)
                dimension1 = [dimension1 ' (' data(1).XUnits ')'];
            end
        end
    end
    dimension2 = 'y coordinate';%'distance';
    if isfield(Props,'NName') && ~isempty(Props.NName)
        dimension2 = Props.NName;
    end
    if isfield(data,'YUnits') && ~isempty(data(1).YUnits)
        dimension2 = [dimension2 ' (' data(1).YUnits ')'];
    end
    if ~isempty(strfind(basicaxestype,'Z'))
        dimension3 = 'elevation';
        if isfield(data,'ZUnits')
            if ~isempty(data(1).ZUnits)
                dimension3 = [dimension3 ' (' data(1).ZUnits ')'];
            end
        elseif isfield(data,'Units') && ~isempty(data(1).Units)
            dimension3 = [dimension3 ' (' data(1).Units ')'];
        end
    else
        dimension3 = PName;
    end
    setaxesprops(Parent,basicaxestype,dimension1,dimension2,dimension3);
end
%==========================================================================
% End of actual plotting
%==========================================================================

Error=0;

if ~isempty(Ops.colourlimits)
    if ChangeCLim
        set(Parent,'clim',Ops.colourlimits)
    end
elseif Ops.symmetriccolourlimits
    lastCLIMSYMM=0;
    if ~isempty(hOldVec)
        xde=get(hOldVec(end),'xdata');
        lastCLIMSYMM=isequal(size(xde),[1 2]) & all(isnan(xde(:)));
    end
    clim=limits(hNewVec,'clim'); clim=max(abs(clim));
    if lastCLIMSYMM
        set(hOldVec(end),'cdata',[-1 1;-1 1]*clim)
        hNew{end+1}=hOldVec(end);
    else
        hNew{end+1}=surface([NaN NaN],[NaN NaN],[NaN NaN;NaN NaN],'cdata',[-1 1;-1 1]*clim);
    end
end

if ~isempty(Ops.colourmap)
    if ischar(Ops.colourmap)
        colormap(qp_colormap(Ops.colourmap));
    else
        ui_message('warning','Colourmap option not yet implemented.');
    end
end

if ~strcmp(Ops.colourbar,'none')
    Chld=get(pfig,'children');
    h=qp_colorbar(Ops.colourbar,'peer',Parent);
    if ~isempty(h)
        set(pfig,'children',[h;Chld(ishandle(Chld) & (Chld~=h))]);

        if ~strcmp(Ops.thresholds,'none')
            if LocLabelClass
                classbar(h,1:length(Thresholds),'labelcolor','label',Thresholds,'plotall')
            else
                classbar(h,1:length(Thresholds),'label',Thresholds,'plotall')
            end
        end
    end
end


if isempty(hNewVec)
    hNewVec=line('xdata',[],'ydata',[],'zdata',[]);
    hNew{end+1}=hNewVec;
end
if isempty(hNewTag)
    hNewTag=sprintf('QPPlotTag [%bx-%bx]',now,rand);
end
set(hNewVec,'hittest','off','tag',hNewTag)
IUD.PlotState=PlotState;
IUD.PlotState.FI=FileInfo;
IUD.PlotState.Handles=hNew;
IUD.XInfo=[];
if isfield(data,'XInfo')
    IUD.XInfo=data.XInfo;
end
if ~isempty(Thresholds)
    IUD.XInfo.Thresholds=Thresholds;
end
set(hNewVec,'userdata',[])
set(hNewVec(1),'userdata',IUD)
for lP=1:length(lParents)
    Parent=lParents(lP);
    if SortObjs
        thisParent=ismember(hNewVec,allchild(Parent));
        hNew_lP=hNewVec(thisParent);
        childs=[PchildBefore{lP};hNew_lP(:);PchildAfter{lP}];
        childlist=allchild(Parent);
        childs(~ismember(childs,childlist))=[];
        childs=cat(1,childlist(~ismember(childlist,childs)),childs);
        set(Parent,'children',childs)
    end
end
