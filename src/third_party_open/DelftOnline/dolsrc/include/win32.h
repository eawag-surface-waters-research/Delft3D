//-----------------------------------------------------------------------------
//  DelftOnline - Microsoft Windows-specific Definitions
//
//  Irv.Elshoff@wldelft.nl
//  13 feb 07
//
//  Copyright (C) 2006-2007, WL | Delft Hydraulics
//-----------------------------------------------------------------------------


#pragma once


#define NOATOM
#define NOCLIPBOARD
#define NOCOLOR
#define NOCOMM
#define NOCTLMGR
#define NODEFERWINDOWPOS
#define NODRAWTEXT
#define NOGDI
#define NOGDICAPMASKS
#define NOHELP
#define NOICONS
#define NOKANJI
#define NOKERNEL
#define NOKEYSTATES
#define NOMB
#define NOMCX
#define NOMEMMGR
#define NOMENUS
#define NOMETAFILE
#define NOMINMAX
#define NOMSG
#define NONLS
#define NOOPENFILE
#define NOPROFILER
#define NORASTEROPS
#define NOSCROLL
#define NOSERVICE
#define NOSHOWWINDOW
#define NOSOUND
#define NOSYSCOMMANDS
#define NOSYSMETRICS
#define NOTEXTMETRIC
#define NOUSER
#define NOVIRTUALKEYCODES
#define NOWH
#define NOWINMESSAGES
#define NOWINOFFSETS
#define NOWINSTYLES
#define OEMRESOURCE


#include <windows.h>


#undef IN
#undef OUT


#ifdef DELFTONLINE_LIB
#   define DLL __declspec (dllexport)
#else
#   define DLL __declspec (dllimport)
#endif

