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

if(WIN32)
    set(openmp_flag /Qopenmp)
endif(WIN32)
if(UNIX)
    if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
        set(CMAKE_Fortran_FLAGS "-O2 -cpp -ffree-line-length-none -fopenmp -fPIC -fcray-pointer ")
    endif()
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
        set(CMAKE_Fortran_FLAGS "-O2 -fpp -qopenmp ")
    endif()
endif(UNIX)

add_subdirectory(external)

add_library(DikesOvertoppingLib
  STATIC
    libDikesOvertopping/utils.f90
    libDikesOvertopping/factorModuleOvertopping.f90
    libDikesOvertopping/mainModuleOvertopping.f90
    libDikesOvertopping/OvertoppingMessages.f90
    libDikesOvertopping/formulaModuleOvertopping.f90
    libDikesOvertopping/ModuleLogging.f90
    libDikesOvertopping/typeDefinitionsOvertopping.f90
    libDikesOvertopping/typeForLoad.f90
    libDikesOvertopping/typeForFactors.f90
    libDikesOvertopping/parametersOvertopping.f90
    libDikesOvertopping/geometryModuleOvertopping.f90
    libDikesOvertopping/omkeerVariantModule.f90
    libDikesOvertopping/waveRunup.f90
    libDikesOvertopping/overtoppingInterface.f90
    libDikesOvertopping/zFunctionsOvertopping.f90
    libDikesOvertopping/submAdjNonHorizBerms.f90
    libDikesOvertopping/submAllocsGeometry.f90
    libDikesOvertopping/submBasicGeometryValidation.f90
    libDikesOvertopping/submCalcHorzProps.f90
    libDikesOvertopping/submCalcSegmentSlopes.f90
    libDikesOvertopping/submCheckCrossSection.f90
    libDikesOvertopping/submCheckSegmentTypes.f90
    libDikesOvertopping/submDetermineSegmentTypes.f90
    libDikesOvertopping/submInitializeGeometry.f90
    libDikesOvertopping/submMergeSequentialBerms.f90
    libDikesOvertopping/submRemoveSegments.f90
    libDikesOvertopping/submSplitCrossSection.f90
    libDikesOvertopping/submAllocOvertopping.f90
    libDikesOvertopping/submCalcOvertopping.f90
    libDikesOvertopping/submCalcOvertoppingSection.f90
    libDikesOvertopping/submCalcWaveOvertopping.f90
    libDikesOvertopping/submInterpResultsSections.f90
    libDikesOvertopping/factorModuleOvertopping.f90
    libDikesOvertopping/submAdjInfluenceFactors.f90
    libDikesOvertopping/submCalcBreakerLimit.f90
    libDikesOvertopping/submCalcWaveOvertopDischarge.f90
    libDikesOvertopping/submCalcWaveProps.f90
    libDikesOvertopping/submCalcWaveRunup.f90
    libDikesOvertopping/submCubicRoots.f90
    libDikesOvertopping/submInnerCalculation.f90
    libDikesOvertopping/submIterationWaveRunup.f90
    libDikesOvertopping/submOmkeerMain.f90
    libDikesOvertopping/submOmkeerValidProfile.f90
    libDikesOvertopping/submAllocVectorsGeom.f90
    libDikesOvertopping/submCopyGeom.f90
    libDikesOvertopping/submRemoveBerms.f90
    libDikesOvertopping/submCheckInputData.f90
    libDikesOvertopping/submCheckModelFactors.f90
)

set_target_properties(DikesOvertoppingLib
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(DikesOvertoppingLib FeedbackDll Feedback HRgeneral)


if(WIN32)
    set(sharedLibName dllDikesOvertopping)
else()
    set(sharedLibName DikesOvertopping)
endif()

add_library(${sharedLibName}
  SHARED
    dllDikesOvertopping/dllOvertopping.f90
)

set_target_properties(${sharedLibName}
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(${sharedLibName} DikesOvertoppingLib FeedbackDll Feedback HRtools)

add_executable(unitTestProgram
    tests/DikesOvertoppingTests/crossSectionsTests.f90
    tests/DikesOvertoppingTests/overtoppingTests.f90
    tests/DikesOvertoppingTests/testHelper.f90
    tests/DikesOvertoppingTests/loadTests.f90
    tests/DikesOvertoppingTests/overtoppingUnitTests.f90
    tests/DikesOvertoppingTests/omkeerVariantTests.f90
    tests/DikesOvertoppingTests/readCrossSectionForTests.f90
    tests/DikesOvertoppingTests/roughnessTests.f90
    tests/DikesOvertoppingTests/dllFewsTests.f90
    tests/DikesOvertoppingTests/dllTests.f90
    tests/unitTests/unitTestsProgram.F90
    external/throwexception.f90
)

target_link_libraries(unitTestProgram ${sharedLibName} DikesOvertoppingLib HRtools FeedbackDll Feedback)

add_executable(runOvertopping
    exe/runOvertopping.f90
)
target_link_libraries(runOvertopping DikesOvertoppingLib FeedbackDll Feedback)

enable_testing()

add_test(
  NAME overtopping-unit-tests
  COMMAND $<TARGET_FILE:unitTestProgram> /s 3
  WORKING_DIRECTORY ../tests/unitTests
)

