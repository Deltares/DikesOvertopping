# Copyright (C) Stichting Deltares and State of the Netherlands 2025. All rights reserved.
#
# This file is part of the Dikes Overtopping Kernel.
#
# The Dikes Overtopping Kernel is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
# All names, logos, and references to "Deltares" are registered trademarks of
# Stichting Deltares and remain full property of Stichting Deltares at all times.
# All rights reserved.
#

# set minimum cmake version
cmake_minimum_required(VERSION 3.28 FATAL_ERROR)

# project name and language
project(DikesOvertopping LANGUAGES Fortran)

#set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

if(WIN32)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /Qfp-speculation=safe")
endif(WIN32)
if(UNIX)
    if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
        set(CMAKE_Fortran_FLAGS "-O2 -cpp -ffree-line-length-none -fPIC -fcray-pointer ")
    endif()
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
        set(CMAKE_Fortran_FLAGS "-O2 -fpp -fp-speculation=safe ")
    endif()
endif(UNIX)

include("${CMAKE_CURRENT_SOURCE_DIR}/src/cmake/version_number.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/src/cmake/generate_product_version.cmake")
set_version_numbers("src/version_number.ini")
generate_product_version(
    VersionFilesOutputVariable
    NAME "Dikes Overtopping"
    ICON "${PROJECT_SOURCE_DIR}/src/default.ico"
    COMPANY_NAME "Deltares and State of the Netherlands"
    VERSION_MAJOR ${ver_major}
    VERSION_MINOR ${ver_minor}
    VERSION_PATCH ${ver_patch}
    ORIGINAL_FILENAME ${ver_git_commit_hash}
)

set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)

add_compile_definitions(maxParallelThreads=96)

add_library(HRtools
  STATIC
    src/core/external/Fortran-Common-Library_src/tools/precision.f90
    src/core/external/Fortran-Common-Library_src/tools/utilities.f90
    src/core/external/Fortran-Common-Library_src/tools/errorMessages.f90
    src/core/external/Fortran-Common-Library_src/tools/equalReals.f90
    src/core/external/throwexception.f90
)

set_target_properties(HRtools
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

add_library(HRgeneral
  STATIC
    src/core/external/versionInfo.f90
    src/core/external/Fortran-Common-Library_src/general/vectorUtilities.f90
    src/core/external/Fortran-Common-Library_src/general/waveParametersUtilities.f90
    src/core/external/Fortran-Common-Library_src/general/angleUtilities.f90
    src/core/external/Fortran-Common-Library_src/ftnunit/ftnunit.f90  
    src/core/external/Fortran-Common-Library_src/ftnunit/ftnunit_hooks_teamcity.f90  
)

target_link_libraries(HRgeneral Feedback)

set_target_properties(HRgeneral
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

add_library(FeedbackDll
  SHARED
    src/core/external/Fortran-Common-Library_src/feedback_dll/feedback_parameters.f90
    src/core/external/Fortran-Common-Library_src/feedback_dll/feedbackDLL_implementation.F90
    src/core/external/Fortran-Common-Library_src/feedback_dll/feedbackDLL.f90
    ${VersionFilesOutputVariable}
)

set_target_properties(FeedbackDll
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

find_package(OpenMP)
if(OpenMP_Fortran_FOUND)
    target_link_libraries(FeedbackDll PUBLIC HRtools OpenMP::OpenMP_Fortran)
endif()

add_library(Feedback
  STATIC
    src/core/external/Fortran-Common-Library_src/feedback/feedback.f90
)

set_target_properties(Feedback
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(Feedback HRtools FeedbackDll)


add_library(DikesOvertoppingLib
  STATIC
    src/core/libDikesOvertopping/utils.f90
    src/core/libDikesOvertopping/factorModuleOvertopping.f90
    src/core/libDikesOvertopping/mainModuleOvertopping.f90
    src/core/libDikesOvertopping/OvertoppingMessages.f90
    src/core/libDikesOvertopping/formulaModuleOvertopping.f90
    src/core/libDikesOvertopping/ModuleLogging.f90
    src/core/libDikesOvertopping/typeDefinitionsOvertopping.f90
    src/core/libDikesOvertopping/typeForLoad.f90
    src/core/libDikesOvertopping/typeForFactors.f90
    src/core/libDikesOvertopping/parametersOvertopping.f90
    src/core/libDikesOvertopping/geometryModuleOvertopping.f90
    src/core/libDikesOvertopping/omkeerVariantModule.f90
    src/core/libDikesOvertopping/waveRunup.f90
    src/core/libDikesOvertopping/overtoppingInterface.f90
    src/core/libDikesOvertopping/zFunctionsOvertopping.f90
    src/core/libDikesOvertopping/submAdjNonHorizBerms.f90
    src/core/libDikesOvertopping/submAllocsGeometry.f90
    src/core/libDikesOvertopping/submBasicGeometryValidation.f90
    src/core/libDikesOvertopping/submCalcHorzProps.f90
    src/core/libDikesOvertopping/submCalcSegmentSlopes.f90
    src/core/libDikesOvertopping/submCheckCrossSection.f90
    src/core/libDikesOvertopping/submCheckSegmentTypes.f90
    src/core/libDikesOvertopping/submDetermineSegmentTypes.f90
    src/core/libDikesOvertopping/submInitializeGeometry.f90
    src/core/libDikesOvertopping/submMergeSequentialBerms.f90
    src/core/libDikesOvertopping/submRemoveSegments.f90
    src/core/libDikesOvertopping/submSplitCrossSection.f90
    src/core/libDikesOvertopping/submAllocOvertopping.f90
    src/core/libDikesOvertopping/submCalcOvertopping.f90
    src/core/libDikesOvertopping/submCalcOvertoppingSection.f90
    src/core/libDikesOvertopping/submCalcWaveOvertopping.f90
    src/core/libDikesOvertopping/submInterpResultsSections.f90
    src/core/libDikesOvertopping/factorModuleOvertopping.f90
    src/core/libDikesOvertopping/submAdjInfluenceFactors.f90
    src/core/libDikesOvertopping/submCalcBreakerLimit.f90
    src/core/libDikesOvertopping/submCalcWaveOvertopDischarge.f90
    src/core/libDikesOvertopping/submCalcWaveProps.f90
    src/core/libDikesOvertopping/submCalcWaveRunup.f90
    src/core/libDikesOvertopping/submCubicRoots.f90
    src/core/libDikesOvertopping/submInnerCalculation.f90
    src/core/libDikesOvertopping/submIterationWaveRunup.f90
    src/core/libDikesOvertopping/submOmkeerMain.f90
    src/core/libDikesOvertopping/submOmkeerValidProfile.f90
    src/core/libDikesOvertopping/submAllocVectorsGeom.f90
    src/core/libDikesOvertopping/submCopyGeom.f90
    src/core/libDikesOvertopping/submRemoveBerms.f90
    src/core/libDikesOvertopping/submCheckInputData.f90
    src/core/libDikesOvertopping/submCheckModelFactors.f90
)

set_target_properties(DikesOvertoppingLib
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(DikesOvertoppingLib FeedbackDll Feedback HRgeneral)
add_dependencies(DikesOvertoppingLib FeedbackDll Feedback HRgeneral)


if(WIN32)
    set(sharedLibName dllDikesOvertopping)
else()
    set(sharedLibName DikesOvertopping)
endif()

add_library(${sharedLibName}
  SHARED
    src/core/dllDikesOvertopping/dllOvertopping.f90
    ${VersionFilesOutputVariable}
)

set_target_properties(${sharedLibName}
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(${sharedLibName} DikesOvertoppingLib FeedbackDll Feedback HRtools)
add_dependencies(${sharedLibName} DikesOvertoppingLib FeedbackDll Feedback HRtools)

add_executable(unitTestProgram
    src/core/tests/DikesOvertoppingTests/crossSectionsTests.f90
    src/core/tests/DikesOvertoppingTests/overtoppingTests.f90
    src/core/tests/DikesOvertoppingTests/testHelper.f90
    src/core/tests/DikesOvertoppingTests/loadTests.f90
    src/core/tests/DikesOvertoppingTests/overtoppingUnitTests.f90
    src/core/tests/DikesOvertoppingTests/omkeerVariantTests.f90
    src/core/tests/DikesOvertoppingTests/readCrossSectionForTests.f90
    src/core/tests/DikesOvertoppingTests/roughnessTests.f90
    src/core/tests/DikesOvertoppingTests/dllFewsTests.f90
    src/core/tests/DikesOvertoppingTests/dllTests.f90
    src/core/tests/unitTests/unitTestsProgram.F90
    src/core/external/throwexception.f90
    ${VersionFilesOutputVariable}
)

target_link_libraries(unitTestProgram ${sharedLibName} DikesOvertoppingLib HRtools FeedbackDll Feedback)

add_executable(runOvertopping
    src/core/exe/runOvertopping.f90
    ${VersionFilesOutputVariable}
)
target_link_libraries(runOvertopping DikesOvertoppingLib FeedbackDll Feedback)

enable_testing()

add_test(
  NAME overtopping-unit-tests
  COMMAND $<TARGET_FILE:unitTestProgram> /s 3
  WORKING_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/src/core/tests/unitTests"
)

