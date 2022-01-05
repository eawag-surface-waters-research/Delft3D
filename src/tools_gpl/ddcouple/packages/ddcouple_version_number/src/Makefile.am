#  Copyright (C)  Stichting Deltares, 2021-2022.
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 3,
#  as published by the Free Software Foundation.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#  contact: delft3d.support@deltares.nl
#  Stichting Deltares
#  P.O. Box 177
#  2600 MH Delft, The Netherlands
#
#  All indications and logos of, and references to registered trademarks
#  of Stichting Deltares remain the property of Stichting Deltares. All
#  rights reserved.

include $(top_srcdir)/common.am
lib_LIBRARIES = libddcouple_version_number.a

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../include 

#-------------------------------------------------------------------------------
#Sources
libddcouple_version_number_a_SOURCES = \
         c2c.c \
         f2c.c \
         version.c

BUILT_SOURCES =  ../include/always \
    ../include/version_number.h

../include/always:
    rm -f ../include/version_number.h

../include/version_number.h:
    $(top_srcdir)/scripts_lgpl/linux/make_revision.sh $(top_srcdir) ../../.. ../include/version_number.ini ../include/version_number.h.svn ../include/version_number.h
#-------------------------------------------------------------------------------
#Libraries

