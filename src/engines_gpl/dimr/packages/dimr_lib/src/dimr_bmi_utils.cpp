//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2017.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: dimr_bmi_utils.cpp 962 2011-10-31 21:52:47Z peele_rh $
// $HeadURL: $
//------------------------------------------------------------------------------
//  dimr bmi utils
//
//------------------------------------------------------------------------------


#if defined (WIN32)
#  include <windows.h>
#endif


#include "bmi.h"
#include "log.h"
#include "dimr_bmi_utils.h"

unsigned int DimrBmiUtils::convertBMILogLevelToDimrLogLevel(int level)
{
	switch ((Level)level)
	{
		case ALL:
			return Log::LOG_DETAIL;
		case DEBUG:
			return Log::DETAIL;
		case INFO:
		default:
			return Log::MINOR;
		case WARNINGS:
			return Log::MAJOR;
		case ERRORS:
			return Log::WARN;
		case FATAL:
			return Log::ALWAYS;
	}
}

Level DimrBmiUtils::convertDimrLogLevelToLogLevel(unsigned int mask)
{
	if (mask & Log::LOG_DETAIL)
		return ALL;
	if (mask & Log::DETAIL)
		return DEBUG;
	if (mask & Log::MINOR)
		return INFO;
	if (mask & Log::MAJOR)
		return WARNINGS;
	if (mask & Log::WARN)
		return ERRORS;
	if (mask & Log::ALWAYS)
		return FATAL;
	return INFO;
}


