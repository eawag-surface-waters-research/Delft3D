function dio_delete(dsh)
%DIO_DELETE  Delete/destroy a DelftIO stream.
%   DIO_DELETE(dsh) deletes the specified DelftIO stream where dsh is a
%   DelftIO stream handle obtained from a DIO_DEFINE call.
%
%   Note: A DIO_DELETE call by the data providing component before the data
%   receiving component has read the data will destroy the data sent!
%
%   See also DIO_DEFINE.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2014 Stichting Deltares.                                 
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
%   $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/tools/matlab/delftio/progsrc/dio_delete.m $
%   $Id: dio_delete.m 34005 2014-05-18 13:10:30Z jagers $

dio_core('delete',dsh);
