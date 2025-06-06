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

set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)

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

add_compile_definitions(maxParallelThreads=96)

include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/version_number.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/generate_product_version.cmake")
set_version_numbers("version_number.ini")
generate_product_version(
    VersionFilesOutputVariable
    NAME "Dikes Overtopping"
    ICON "${PROJECT_SOURCE_DIR}/default.ico"
    COMPANY_NAME "Deltares and State of the Netherlands"
    VERSION_MAJOR ${ver_major}
    VERSION_MINOR ${ver_minor}
    VERSION_PATCH ${ver_patch}
    ORIGINAL_FILENAME ${ver_git_commit_hash}
)

add_library(HRtools
  STATIC
    Fortran-Common-Library_src/tools/precision.f90
    Fortran-Common-Library_src/tools/utilities.f90
    Fortran-Common-Library_src/tools/errorMessages.f90
    Fortran-Common-Library_src/tools/equalReals.f90
    throwexception.f90
)

set_target_properties(HRtools
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

add_library(HRgeneral
  STATIC
    versionInfo.f90
    Fortran-Common-Library_src/general/vectorUtilities.f90
    Fortran-Common-Library_src/general/waveParametersUtilities.f90
    Fortran-Common-Library_src/general/angleUtilities.f90
    Fortran-Common-Library_src/ftnunit/ftnunit.f90  
    Fortran-Common-Library_src/ftnunit/ftnunit_hooks_teamcity.f90  
)

target_link_libraries(HRgeneral Feedback)

set_target_properties(HRgeneral
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

add_library(FeedbackDll
  SHARED
    Fortran-Common-Library_src/feedback_dll/feedback_parameters.f90
    Fortran-Common-Library_src/feedback_dll/feedbackDLL_implementation.F90
    Fortran-Common-Library_src/feedback_dll/feedbackDLL.f90
    ${VersionFilesOutputVariable}
)

set_target_properties(FeedbackDll
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)
target_link_libraries(FeedbackDll HRtools)

if(WIN32)
    set_target_properties(FeedbackDll PROPERTIES COMPILE_FLAGS ${openmp_flag})
    target_link_options(FeedbackDll PRIVATE ${openmp_flag})
endif(WIN32)

add_library(Feedback
  STATIC
    Fortran-Common-Library_src/feedback/feedback.f90
)

set_target_properties(Feedback
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(Feedback HRtools FeedbackDll)

