function Info=bct_io(cmd,varargin)
%BCT_IO Read/write boundary condition tables.
%   INFO = BCT_IO('READ',FILENAME) opens the specified file and read all
%   its time series into memory. The returning structure INFO contains a
%   Table structure that list the data of each time series stored in that
%   particular file.
%
%   BCT_IO('WRITE',FILENAME,INFO)

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

switch lower(cmd)
    case 'read'
        Info=Local_read_bct(varargin{:});
    case 'write'
        OK=Local_write_bct(varargin{:});
        if nargout>0
            Info=OK;
        elseif OK<0
            error('Error writing file')
        end
    otherwise
        error('Unknown bct_io command: ''%s''.',cmd)
end


function Info=Local_read_bct(filename)

fid=fopen(filename,'r');
Info.Check='NotOK';
Info.FileName=filename;
Info.NTables=0;

floc=ftell(fid);
Line=fgetl(fid);
while ischar(Line) && ~isempty(Line) && Line(1)=='#'
    floc=ftell(fid);
    Line=fgetl(fid);
end

i=1;
NPar=0;
try
    while ~feof(fid)
        [keyw,remainder] = strtok(Line);
        switch keyw
            case 'table-name'
                Info.Table(i).Name = deblank(strextract(remainder));
            case 'contents'
                Info.Table(i).Contents = deblank(strextract(remainder));
            case 'location'
                Info.Table(i).Location = deblank(strextract(remainder));
            case 'time-function'
                Info.Table(i).TimeFunction=deblank(strextract(remainder));
            case 'reference-time'
                Info.Table(i).ReferenceTime=sscanf(remainder,'%i',1);
            case 'time-unit'
                Info.Table(i).TimeUnit=deblank(strextract(remainder));
            case 'interpolation'
                Info.Table(i).Interpolation=deblank(strextract(remainder));
            case 'parameter'
                NPar=NPar+1;
                [Info.Table(i).Parameter(NPar).Name,Part2]=strextract(remainder);
                Info.Table(i).Parameter(NPar).Name=deblank(Info.Table(i).Parameter(NPar).Name);
                Unit=strfind(Part2,'''');
                if isempty(Unit) % no quotes
                    Unit=strfind(lower(Part2),'unit');
                    if isempty(Unit) % no string 'unit'
                        % nothing to do, Part2 most likely already contains the unit string
                    else % remove 'unit'
                        Part2=Part2((Unit(1)+4):end);
                    end
                    Info.Table(i).Parameter(NPar).Unit=deblank(Part2);
                else
                    Part2=Part2(Unit(1):end);
                    Info.Table(i).Parameter(NPar).Unit=deblank(strextract(Part2));
                end
            case 'records-in-table'
                NRec=fscanf(fid,'%i',1);
            otherwise
                if NPar==0
                    error('No parameter keywords found before data.')
                end
                fseek(fid,floc,-1);
                Info.Table(i).Data=transpose(fscanf(fid,'%f',[NPar inf]));
                i=i+1;
                NPar=0;
                Info.NTables=Info.NTables+1;
        end
        floc=ftell(fid);
        Line=fgetl(fid);
    end
catch
    fclose(fid);
    error('Error interpreting line:\n%s',Line)
end
fclose(fid);
if Info.NTables==0
    error('No tables in bct file?')
end
Info.Check='OK';

function [Str1,remainder]=strextract(Str,Quote)
if nargin==1
    Quote='''';
end
Q=strfind(Str,Quote);
if isempty(Q)
    error('No quote found in ''%s''',Str)
end
if length(Q)<1
    error('Too few quotes found in ''%s''',Str)
end
Str = Str(Q(1)+1:end);
Q = Q(2:end)-Q(1);
%
i=1;
done=0;
while ~done
    if i<length(Q)
        if Q(i+1)==Q(i) % two quotes representing just one quote
            i=i+2;
        else
            done=1;
        end
    elseif i==length(Q)
        done=1;
    else
        warning('No ending quote found in string ''%s''.',Str)
        Str1=strrep(Str,[Quote Quote],Quote);
        remainder='';
        return
    end
end

Str1=strrep(Str(1:(Q(i)-1)),[Quote Quote],Quote);
remainder=Str((Q(i)+1):end);


function OK=Local_write_bct(filename,Info)
fid=fopen(filename,'w');
% When the file is written using a fixed line/record length this is
% shown in the first line
%fprintf(fid,'# %i\n',linelength);

for i = 1:length(Info.Table)
    if isfield(Info.Table,'Name') && ~isempty(Info.Table(i).Name)
        fprintf(fid,'table-name          ''%s''\n',Info.Table(i).Name);
    end
    if isfield(Info.Table,'Contents') && ~isempty(Info.Table(i).Contents)
        fprintf(fid,'contents            ''%s''\n',Info.Table(i).Contents);
    end
    if isfield(Info.Table,'Location') && ~isempty(Info.Table(i).Location)
        fprintf(fid,'location            ''%s''\n',Info.Table(i).Location);
    end
    fprintf(fid,'time-function       ''%s''\n',Info.Table(i).TimeFunction);
    fprintf(fid,'reference-time       %i\n',Info.Table(i).ReferenceTime);
    fprintf(fid,'time-unit           ''%s''\n',Info.Table(i).TimeUnit);
    fprintf(fid,'interpolation       ''%s''\n',Info.Table(i).Interpolation);
    
    for j=1:length(Info.Table(i).Parameter)
        fprintf(fid,'parameter           ''%s'' unit ''%s''\n', ...
            Info.Table(i).Parameter(j).Name, ...
            Info.Table(i).Parameter(j).Unit);
    end
    
    fprintf(fid,'records-in-table     %i\n',size(Info.Table(i).Data,1));
    fprintf(fid,[repmat('%15.7e ',1,length(Info.Table(i).Parameter)) '\n'], ...
        transpose(Info.Table(i).Data));
end
fclose(fid);
OK=1;
