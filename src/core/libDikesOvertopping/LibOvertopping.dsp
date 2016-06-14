# Microsoft Developer Studio Project File - Name="LibOvertopping" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=LibOvertopping - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "LibOvertopping.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "LibOvertopping.mak" CFG="LibOvertopping - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "LibOvertopping - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "LibOvertopping - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "LibOvertopping - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\CVF_libs\Release"
# PROP Intermediate_Dir "..\..\..\CVF_libs\Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "LibOvertopping - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\CVF_libs\Debug"
# PROP Intermediate_Dir "..\..\..\CVF_libs\Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "LibOvertopping - Win32 Release"
# Name "LibOvertopping - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\factorModuleRTOovertopping.f90
NODEP_F90_FACTO=\
	"..\..\..\libs\Release\geometryModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\OvertoppingMessages.mod"\
	"..\..\..\libs\Release\precision.mod"\
	"..\..\..\libs\Release\typeDefinitionsRTOovertopping.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\formulaModuleRTOovertopping.f90
NODEP_F90_FORMU=\
	"..\..\..\libs\Release\OvertoppingMessages.mod"\
	"..\..\..\libs\Release\typeDefinitionsRTOovertopping.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\geometryModuleRTOovertopping.f90
NODEP_F90_GEOME=\
	"..\..\..\libs\Release\errorMessages.mod"\
	"..\..\..\libs\Release\formulaModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\overtoppingInterface.mod"\
	"..\..\..\libs\Release\OvertoppingMessages.mod"\
	"..\..\..\libs\Release\typeDefinitionsRTOovertopping.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\mainModuleRTOovertopping.f90
NODEP_F90_MAINM=\
	"..\..\..\libs\Release\factorModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\formulaModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\geometryModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\OvertoppingMessages.mod"\
	"..\..\..\libs\Release\typeDefinitionsRTOovertopping.mod"\
	"..\..\..\libs\Release\waveRunup.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleLogging.f90
NODEP_F90_MODUL=\
	"..\..\..\libs\Release\feedback_parameters.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\omkeerVariantModule.f90
NODEP_F90_OMKEE=\
	"..\..\..\libs\Release\equalReals.mod"\
	"..\..\..\libs\Release\geometryModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\ModuleLogging.mod"\
	"..\..\..\libs\Release\overtoppingInterface.mod"\
	"..\..\..\libs\Release\OvertoppingMessages.mod"\
	"..\..\..\libs\Release\typeDefinitionsRTOovertopping.mod"\
	"..\..\..\libs\Release\vectorUtilities.mod"\
	"..\..\..\libs\Release\zFunctionsWTIOvertopping.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\overtoppingInterface.f90
NODEP_F90_OVERT=\
	"..\..\..\libs\Release\precision.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\OvertoppingMessages.f90
NODEP_F90_OVERTO=\
	"..\..\..\libs\Release\utilities.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\typeDefinitionsRTOovertopping.f90
NODEP_F90_TYPED=\
	"..\..\..\libs\Release\precision.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\waveRunup.f90
NODEP_F90_WAVER=\
	"..\..\..\libs\Release\factorModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\formulaModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\geometryModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\ModuleLogging.mod"\
	"..\..\..\libs\Release\OvertoppingMessages.mod"\
	"..\..\..\libs\Release\typeDefinitionsRTOovertopping.mod"\
	"..\..\..\libs\Release\utilities.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\zFunctionsWTIOvertopping.f90
NODEP_F90_ZFUNC=\
	"..\..\..\libs\Release\equalReals.mod"\
	"..\..\..\libs\Release\geometryModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\mainModuleRTOovertopping.mod"\
	"..\..\..\libs\Release\overtoppingInterface.mod"\
	"..\..\..\libs\Release\OvertoppingMessages.mod"\
	"..\..\..\libs\Release\precision.mod"\
	"..\..\..\libs\Release\typeDefinitionsRTOovertopping.mod"\
	"..\..\..\libs\Release\vectorUtilities.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# End Target
# End Project
