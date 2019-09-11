//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2019.
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
// $Id: xmltree.h 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/utils_lgpl/d_hydro_lib/include/xmltree.h $
//------------------------------------------------------------------------------
//  d_hydro
//  Tree-representation of an XML file - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------


#pragma once

#include <string>
#include "IXmlTree.h"


using namespace std;
#include <vector>


class XmlTree : public IXmlTree {
	public:
	XmlTree(
		FILE * input
	);

	XmlTree(
		XmlTree * parent,
		const char * name
	);

	~XmlTree();

	void
		AddAttrib(
			const char * name,
			const char * value
		) override;

	void
		AddChild(
			IXmlTree * child
		) override;

	void
		ExpandEnvironmentVariables(
		) override;

	void
		ExpandEnvironmentVariables(
			int instance
		) override;

	bool
		GetBoolAttrib(
			const char * name
		) override;

	long int
		GetIntegerAttrib(
			const char * name
		) override;

	double
		GetFloatAttrib(
			const char * name
		);

	XmlTree *
		Lookup(
			const char * pathname
		) override;

	XmlTree *
		Lookup(
			const char * pathname,
			int instance
		) override;

	int
		Lookup(
			const char * pathname,
			int instance,
			keyValueLL * &kvlist 					// key-value pairs linked list
		) override;

	const char *
		GetAttrib(
			const char * name
		) override;

	const char *
		GetElement(
			const char * name
		) override;

	bool
		GetBoolElement(
			const char * name,
			bool defaultValue
		) override;

	void
		Print(
			void
		) override;

private:
	void
		init(
			void
		);

	void
		print(
			int level
		);

	string
		SubstEnvVar(
			string instr
		);

	static string
		EnvSubst(
			std::string instr
		);

public:
	static const int maxCharData = 1000000;  // maximum size of an XML character data block
	static const int maxPathname = 2560;     // maximum length of a full path name

	XmlTree *   parent;
	char *      name;
	char *      pathname;

	vector<char *>   attribNames;
	vector<char *>   attribValues;

	vector<XmlTree *>   children;

	char *      charData;
	int         charDataLen;

private:
};


