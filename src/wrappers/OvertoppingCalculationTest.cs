// Copyright (C) Stichting Deltares and State of the Netherlands 2025. All rights reserved.
//
// This file is part of the Dikes Overtopping Kernel.
//
// The Dikes Overtopping Kernel is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
//
// All names, logos, and references to "Deltares" are registered trademarks of
// Stichting Deltares and remain full property of Stichting Deltares at all times.
// All rights reserved.

﻿using NUnit.Framework;

namespace TestWrapper
{
    [TestFixture]
    public class OvertoppingCalculationTest
    {
        public static int Main()
        {
            TestOvertopping();
            TestOmkeerVariant();
            TestOvertoppingValidation();
            TestOvertoppingValidationGeometry();
            TestOvertoppingValidationMultiple();
            return 0;
        }

        [Test]
        public static void TestOvertopping()
        {
            const double zExpected1A = 11.725;
            const double zExpected1B = 701.48866;
            const double margin = 0.00001;

            const int npoints = 3;
            var xcoords = new double[npoints];
            var ycoords = new double[npoints];
            var roughness = new double[npoints];

            const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingInput
                {
                    FactorDeterminationQnFn = 2.3,
                    FactorDeterminationQbFb = 4.3,
                    Mz2 = 1.0,
                    Fshallow = 0.92,
                    ComputedOvertopping = 1,
                    CriticalOvertopping = 1,
                    Relaxationfactor = 1.0,
                    ReductionFactorForeshore = 0.5
                };

            const double criticalOvertoppingRate = 0.001;

            for (int i = 0; i < npoints; i++)
            {
                xcoords[i] = 5*(i + 1);
                ycoords[i] = 3 + 2*(i + 1);
                roughness[i] = 1;
            }

            const double normal = 60.0; // degrees

            var load1 = new OvertoppingLoadStruct
            { WaterLevel = 5.5, Direction = 50, Height = 1, Period = 4.0 };

            var result = OvertoppingFortranAccess.GetDischarge(load1, normal, xcoords, ycoords, roughness, dikeHeight, modelFactors);

            // this is not a benchmark, it only checks that the results do not change within 7 significant digits
            Assert.AreEqual(1.519737, result.Z2, 0.000001);
            Assert.AreEqual(8.089025E-09d, result.Qo, 1.0E-15);

            var z = OvertoppingFortranAccess.GetZValue(criticalOvertoppingRate, modelFactors, result.Qo);

            Assert.AreEqual(zExpected1A, z, margin, "Z value from dllOvertopping.dll");

            // no waves test:
            var load2 = new OvertoppingLoadStruct
            { WaterLevel = 5.5, Direction = 50, Height = 0, Period = 4.0 };

            result = OvertoppingFortranAccess.GetDischarge(load2, normal, xcoords, ycoords, roughness, dikeHeight, modelFactors);
            z = OvertoppingFortranAccess.GetZValue(criticalOvertoppingRate, modelFactors, result.Qo);
            Assert.AreEqual(z, zExpected1B, margin, "Z value from dllOvertopping.dll; no waves test");
        }

        [Test]
        public static void TestOvertoppingValidation()
        {
            const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingInput
            {
                FactorDeterminationQnFn = 2.3,
                FactorDeterminationQbFb = 4.3,
                Mz2 = 1.0,
                Fshallow = 0.92,
                ComputedOvertopping = 1,
                CriticalOvertopping = 1,
                Relaxationfactor = 1.0,
                ReductionFactorForeshore = 0.5
            };

            var xcoords = new double[]{ 0, 10, 20, 30, 40 };
            var ycoords = new double[] {-5, 0, 5, 4, 0};
            var roughness = new [] {0.5, 0.5, 0.5, 0.5};

            string[] msg;
            var result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);

            Assert.IsFalse(result, "validation");
            Assert.AreEqual(msg[0], "FOUT:Verticale coordinaten mogen niet afnemen.   5.00 en    4.00 doen dat wel.", "validation message");
            Assert.AreEqual(msg[1], "FOUT:Verticale coordinaten mogen niet afnemen.   4.00 en    0.00 doen dat wel.", "validation message");
        }

        [Test]
        public static void TestOvertoppingValidationMultiple()
        {
            const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingInput
            {
                FactorDeterminationQnFn = 2.3,
                FactorDeterminationQbFb = 4.3,
                Mz2 = 1.0,
                Fshallow = -0.92,
                ComputedOvertopping = 1,
                CriticalOvertopping = 1,
                Relaxationfactor = 1.0,
                ReductionFactorForeshore = 0.5
            };

            var xcoords = new double[] { 0, 10, 20, 30, 40 };
            var ycoords = new double[] { -5, 0, 5, 4, 0 };
            var roughness = new[] { 0.5, 0.5, 0.5, 0.5 };

            string[] msg;
            var result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);

            Assert.IsFalse(result, "validation");
            Assert.AreEqual(msg[0], "FOUT:Verticale coordinaten mogen niet afnemen.   5.00 en    4.00 doen dat wel.", "validation message");
            Assert.AreEqual(msg[1], "FOUT:Verticale coordinaten mogen niet afnemen.   4.00 en    0.00 doen dat wel.", "validation message");
            Assert.AreEqual(msg[2], "FOUT:Model factor fS (ondiepe golven) kleiner dan  0.000", "validation message");
        }

        [Test]
        public static void TestOmkeerVariant()
        {
            const int npoints = 3;
            var xcoords = new double[npoints];
            var ycoords = new double[npoints];
            var roughness = new double[npoints];

            //const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingInput
            {
                FactorDeterminationQnFn = 2.3,
                FactorDeterminationQbFb = 4.3,
                Mz2 = 1.0,
                Fshallow = 0.92,
                ComputedOvertopping = 1,
                CriticalOvertopping = 1,
                Relaxationfactor = 1.0,
                ReductionFactorForeshore = 0.5
            };

            for (int i = 0; i < npoints; i++)
            {
                xcoords[i] = 5 * (i + 1);
                ycoords[i] = 3 + 2 * (i + 1);
                roughness[i] = 1;
            }

            const double normal = 60.0; // degrees

            var load = new OvertoppingLoadStruct
                {WaterLevel = 5.5, Direction = 50, Height = 1, Period = 4.0};

            const double discharge = 1e-8;

            var dikeHeight = OvertoppingFortranAccess.OmkeerVariant(load, discharge, normal, xcoords, ycoords, roughness, modelFactors);

            // this is not a benchmark, it only checks that the results do not change within 7 significant digits
            Assert.AreEqual(9.055, dikeHeight, 0.01);
        }

        [Test]
        public static void TestOvertoppingValidationGeometry()
        {
            // Note: this test proves that validation by the Overtopping fails is some cases (mainly when checking allowed angles for slopes and berms) 
            const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingInput
            {
                FactorDeterminationQnFn = 0.4,
                FactorDeterminationQbFb = 0.4,
                Mz2 = 1.0,
                Fshallow = 0.92,
                ComputedOvertopping = 1,
                CriticalOvertopping = 1,
                Relaxationfactor = 1.0,
                ReductionFactorForeshore = 0.34
            };
            string[] msg;

            // Only one point so error expected
            var xcoords = new double[] { 0 };
            var ycoords = new double[] { -5 };
            var roughness = new[] { 0.5 };

            var result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);
            // This validation does work
            Assert.IsFalse(result, "validation");
            Assert.AreEqual(1, msg.Length, "number of validation messages");
            Assert.AreEqual(msg[0], "FOUT:Aantal coordinaten dijk doorsnede is kleiner dan 2");

            xcoords = new double[] { 0, 10, 12, 13, 15, 18, 20, 22 };
            ycoords = new double[] { 5, 10, 11, 11.2, 11.4, 12, 13, 13.8 };
            roughness = new[] { 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 };

            result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);
            Assert.IsFalse(result, "validation");
            Assert.AreEqual(1, msg.Length, "number of validation messages");
            Assert.AreEqual(msg[0], "FOUT:Dijk segment is van ander type dan berm segment of helling segment");

            // correct number of points, slope far too steep
            xcoords = new double[] { 0, 1 };
            ycoords = new double[] { -5, 10 };
            roughness = new[] { 0.5, 0.5 };

            result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);
            Assert.IsFalse(result, "validation");
            Assert.AreEqual(1, msg.Length, "number of validation messages");
            Assert.AreEqual(msg[0], "FOUT:Dijk segment is van ander type dan berm segment of helling segment", "validation message 1");

            // correct number of points, no slope at all
            xcoords = new double[] { 0, 1 };
            ycoords = new double[] { 10.00, 10.00 };
            roughness = new[] { 0.5, 0.5 };

            // This validation does work.
            result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);
            Assert.IsFalse(result, "validation");
            Assert.AreEqual(1, msg.Length, "number of validation messages");
            Assert.AreEqual(msg[0], "FOUT:Eerste segment is een berm. Dat is niet toegestaan."); //TODO berm as slope == 0.0 ?

            // correct number of points, too shallow slope
            xcoords = new double[] { 0, 10000 };
            ycoords = new double[] { 10, 11 };
            roughness = new[] { 0.5, 0.5 };

            // NOTE: This should have failed as slope clearly exceeds min of 1:8
            result = OvertoppingFortranAccess.Validate(xcoords, ycoords, roughness, dikeHeight, modelFactors, out msg);
            Assert.IsFalse(result, "validation");
            Assert.AreEqual(1, msg.Length, "number of validation messages");
            Assert.AreEqual(msg[0], "FOUT:Eerste segment is een berm. Dat is niet toegestaan."); //TODO berm as slope == 0.0 ?
        }
    }
}
