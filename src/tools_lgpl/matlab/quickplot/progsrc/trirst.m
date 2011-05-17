function varargout=trirst(cmd,varargin),
%TRIRST Read/write Delft3D-FLOW restart file.
%   [H,U,V,...]=TRIRST('read',FILENAME,SIZE)
%   or
%   [H,U,V,...]=TRIRST('read',FILENAME,GRID)
%   where GRID was generated by WLGRID.
%   For a restart file of 3D simulations you need to specify
%   the number of layers per field as an array NLAYERS:
%   [H,U,V,...]=TRIRST('read',FILENAME,GRID,NLAYERS)
%   where NLAYERS=[NLAYERH , NLAYERU , NLAYERV , ...]
%
%   TRIRST('write',FILENAME,H,U,V,...)
%   TRIRST('write',FILENAME,PLATFORM,H,U,V,...)
%   where PLATFORM can be 'pc' or 'unix'. PLATFORM is
%   by default equal to the computer on which
%   MATLAB is running.
%

%   X=TRIRST('read',FILENAME,GRID,'all')
%   reads all datasets in the file assuming NLAYERS=1
%   and returns them in a structure with one field: Data.

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

if nargin==0,
   if nargout>0,
      varargout=cell(1,nargout);
   end;
   return;
end;

switch lower(cmd),
   case 'read',
      if nargout>0,
         varargout=cell(1,nargout);
         [varargout{:}]=Local_rstread(varargin{:});
      else,
         Local_rstread(varargin{:});
      end;
   case 'write',
      OK=Local_rstwrite(varargin{:});
      if nargout>0,
         varargout={OK};
      end;
   otherwise,
      error('Unknown command');
end;



function varargout=Local_rstread(filename,dimvar,threeD),
% RSTREAD reads data from restart file.
%
%    [H,U,V,...]=RSTREAD(FILENAME,DIMVAR,NLAYERS)
%

if nargin<2,
   warning('No size or grid specified.');
   return;
end;

NRequested=nargout;
varargout=cell(1,NRequested);
if isstruct(dimvar), % new grid format G.X, G.Y, G.Enclosure
   dim=size(dimvar.X)+1;
elseif iscell(dimvar), % old grid format {X Y Enclosure}
   dim=size(dimvar{1})+1;
else,
   dim=dimvar;
end;

fid=fopen(filename,'r','b'); % Try UNIX format ...
T=fread(fid,1,'int32');
if (4*prod(dim))~=T, % If not a match ...
   fclose(fid);
   fid=fopen(filename,'r','l'); % Try PC format ...
   Tb=T;
   T=fread(fid,1,'int32');
end;
if (4*prod(dim))~=T, % If not a match ...
   fclose(fid);
   error(sprintf('Specified size %ix%i=%i does not match\nrecord length %i or %i in file.',dim,prod(dim),T/4,Tb/4));
end;

if nargin==2,
   threeD=1;
elseif isequal(lower(threeD),'all') & NRequested==1,
   threeD=1;
   NRequested=inf;
else
   threeD=threeD(:);
   if length(threeD)<NRequested,
      ui_message('warning',{'Length of NLAYERS array is less than expected number of fields.','Using NLAYERS=1 for remaining fields.'});
   end;
   threeD(end+1)=1;
end;

i=0;
ReadMore=2;
Y=[];
while ReadMore>1,
   i=i+1;
   nlayer=threeD(min(i,length(threeD)));
   if isfinite(NRequested)
      Y=[];
   end
   for l=1:nlayer,
      X=fread(fid,dim,'float32');
      if ~isequal(size(X),dim),
         fclose(fid);
         Msg=sprintf('Out of data while reading layer %i of record %i.',l,i);
         if (i>NRequested),
            ui_message('warning',Msg);
            return;
         else,
            error(Msg);
         end;
      end;
      if isfinite(NRequested)
         Y(1:dim(1),1:dim(2),l)=X;
      else
         Y(i).Data=X;
      end
      [T,ReadMore]=fread(fid,2,'int32');
   end;
   if isfinite(NRequested)
      varargout{i}=Y;
   end
end;
if ~isfinite(NRequested)
   varargout{1}=Y;
end
if (i>NRequested) & (NRequested~=0),
   ui_message('warning',sprintf('%i more data field(s) in the file.\n',i-NRequested));
elseif (i<NRequested) & isfinite(NRequested),
   error(sprintf('Out of data while reading layer %i of record %i.\n',l,i));
end;
fclose(fid);


function OK=Local_rstwrite(filename,varargin),
% RSTWRITE writes data to a restart file.
%
%    RSTWRITE(FILENAME,H,U,V,...)
%    RSTWRITE(FILENAME,'PC',H,U,V,...)
%    RSTWRITE(FILENAME,'UNIX',H,U,V,...)
%

OK=0;
if nargin<4,
   error('Not enough input arguments.');
end;
if ischar(varargin{1}),
   PC=varargin{1};
   Data=varargin(2:end);
else,
   PC=computer;
   Data=varargin;
end;
sz1 = cellfun('size',Data,1);
sz2 = cellfun('size',Data,2);
if ~all(sz1==sz1(1)) | ~all(sz2==sz2(1)),
   error('Data sets should be of equal size');
end;
switch lower(PC);
   case {'pc','pcwin','dos','windows','l','ieee-le'},
      fid=fopen(filename,'w','l');
   case {'hp','sg','sgi','sgi64','unix','b','sol','sol2','ieee-be'},
      fid=fopen(filename,'w','b');
   otherwise,
      error(sprintf('Unsupported file format: %s.',PC));
end;
T=4*prod(size(Data{1}));
for i=1:length(Data),
   szData=size(Data{i});
   nlayer=prod(szData(3:end));
   for l=1:nlayer,
      fwrite(fid,T,'int32');
      fwrite(fid,Data{i}(:,:,l),'float32');
      fwrite(fid,T,'int32');
   end;
end;
fclose(fid);
OK=1;
