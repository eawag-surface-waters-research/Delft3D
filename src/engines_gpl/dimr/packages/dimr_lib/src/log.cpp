//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2017.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
// $Id: log.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: $
//------------------------------------------------------------------------------
//  Log Object - Implementation
//
//  Irv.Elshoff@Deltares.NL
//  25 oct 11
//------------------------------------------------------------------------------


#include "log.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>


#if defined (WIN32)
#   define strdup _strdup
#endif

Log::Log( FILE *  output, Clock * clock, Mask mask,	Mask feedbackMask) {
	this->output = output;
	this->clock = clock;
	this->mask = mask;
	this->feedbackMask = feedbackMask;
	this->redirectFile = NULL;

	this->writeCallback = NULL;
	this->externalLogger = NULL;

	if (pthread_key_create(&this->thkey, NULL) != 0)
		throw new Exception(true, "Pthreads error in Log: Cannot create thread-specific key: %s", strerror(errno));
	if (pthread_setspecific(this->thkey, NULL) != 0)
		throw new Exception(true, "Pthreads error in Log constructor: Cannot set thread-specific key: %s", strerror(errno));
}


Log::~Log( void ) {
	this->writeCallback = NULL;
	// nothing to do
}


//------------------------------------------------------------------------------


Log::Mask Log::GetMask( void ) {
	return this->mask;
}


void Log::SetMask( Mask mask ) {
	this->mask = mask;
	this->Write(Log::MAJOR, 0, "Log mask set to 0x%08x", this->mask);
}


Log::Mask Log::GetFeedbackLevel( void ) {
	return this->feedbackMask;
}


void Log::SetFeedbackLevel( Mask feedbackMask) {
	this->feedbackMask = feedbackMask;
	this->Write(Log::MAJOR, 0, "Feedback Level mask set to 0x%08x", this->feedbackMask);
}


void Log::RegisterThread( const char * id ) {
	char * idCopy = strdup(id);
	if (pthread_setspecific(this->thkey, (void *)idCopy) != 0)
		throw new Exception(true, "Pthreads error in Log::RegisterThread: Cannot set thread-specific key: %s", strerror(errno));
}


void Log::RenameThread( const char * id ) {
	this->UnregisterThread();
	this->RegisterThread(id);
}


void Log::UnregisterThread( void ) {
	char * id = (char *)pthread_getspecific(this->thkey);
	if (id == NULL)
		throw new Exception(true, "Log thread key not set in UnregisterThread");

	free(id);
}


bool Log::Write( Mask mask, int rank, const char *  format, ... ) {
	const int bufsize = 256 * 1024;
	char * buffer = new char[bufsize]; // really big temporary buffer, just in case

	va_list arguments;
	va_start(arguments, format);
	int len = vsnprintf(buffer, bufsize - 1, format, arguments);
	va_end(arguments);
	buffer[bufsize - 1] = '\0';

	if (this->externalLogger){
		//Level level = Dimr::convertDimrLogLevelToLogLevel((int)(mask));
		this->externalLogger(INFO, buffer);
	}

	if ((int)this->mask - (int)mask < 0) {
		return false;
	}

	char * clock = new char[100];
	clock[0] = '\0';
	this->clock->Now(clock);
	string clockstring;
	sscanf(clock, "%s", clockstring);
	time_t ttNow = time(0);
	tm * ptmNow;

	ptmNow = localtime(&ttNow);

	//year
	string year = to_string(1900 + ptmNow->tm_year);

	// month
	string month;
	if (ptmNow->tm_mon < 9)
		//Fill in the leading 0 if less than 10
		month = "0" + to_string(1 + ptmNow->tm_mon);
	else
		month = (1 + ptmNow->tm_mon);

	char * seconds = new char[strlen(clock) + 1];
	strcpy(seconds, clock);
	char * remainder = strchr(seconds, '.');
	if (remainder == NULL)
		remainder = (char *) "";
	else
		*remainder++ = '\0';
	int sec = atoi(seconds);
	int msec = atoi(remainder);
	//day
	int d = (int)(sec / 60 / 60 / 24);
	string day = to_string(d);
	int h = (int)((sec - (d * 60 * 60 * 24)) / 60 / 60);
	string hours = to_string(h);
	int m = (int)(sec - (d * 60 * 60 * 24) - (h * 60 * 60)) / 60;
	string mins = to_string(m);
	int s = (int)(sec - (d * 60 * 60 * 24) - (h * 60 * 60) - (m * 60));
	string secs = to_string(s);
	string msecs = to_string(msec);


	char * threadID = (char *)pthread_getspecific(this->thkey);
	if (threadID == NULL)
		threadID = "<anonymous>";

	if (redirectFile != NULL) {	
		// Append to file:
		FILE * fp;
		fp = fopen(redirectFile, "a");
		fprintf(fp, "Dimr [%s-%s-%s %s:%s:%s.%s] #%d >> %s\n",
			year.c_str(),
			month.c_str(),
			day.c_str(),
			hours.c_str(),
			mins.c_str(),
			secs.c_str(),
			msecs.c_str(),
			rank,
			buffer
			);
		fclose(fp);
	}
	else {
		// Write to stdout:
		fprintf(this->output, "Dimr [%s-%s-%s %s:%s:%s.%s] #%d >> %s\n",
			year.c_str(),
			month.c_str(),
			day.c_str(),
			hours.c_str(),
			mins.c_str(),
			secs.c_str(),
			msecs.c_str(),
			rank,
			buffer
			);
		fflush(this->output);
	}

	// Write to Callback (if registered)
	// Use separate write Mask
	if (this->writeCallback && (int)this->feedbackMask - (int)mask >= 0){
		this->writeCallback(&clock[0], buffer, mask);
	}

	delete[] buffer;
	delete[] clock;
	return true;
}


void Log::SetWriteCallBack( WriteCallback writeCallback ) {
	this->writeCallback = writeCallback;
	this->Write(Log::MAJOR, 0, "WriteCallBack is set");
}


void Log::SetExternalLogger(Logger logger){
	this->externalLogger = logger;
	this->Write(Log::MAJOR, 0, "External logger is set");
}


