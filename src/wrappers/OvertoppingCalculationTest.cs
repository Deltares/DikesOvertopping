using Deltares.Geotechnics;
using Deltares.Overtopping.Calculation;
using NUnit.Framework;

namespace Deltares.Overtopping.Test
{
    [TestFixture]
    public class OvertoppingCalculationTest
    {
        [Test]
        public void TestOvertopping()
        {
            const double zExpected1A = 29.88985;
            const double zExpected1B = 701.48866;
            const double zExpected2 = -6.43984;
            const double margin = 0.00001;

            const int npoints = 3;
            var xcoords = new double[npoints];
            var ycoords = new double[npoints];
            var roughness = new double[npoints];

            const double dikeHeight = 9.1;
            var modelFactors = new OvertoppingFortranAccess.OvertoppingModelFactors
                {
                    FactorDeterminationQnFn = 2.3,
                    FactorDeterminationQbFb = 4.3,
                    Frunup1 = 1.75,
                    Frunup2 = 4.30,
                    Frunup3 = 1.60,
                    Fshallow = 0.21,
                    ComputedOvertopping = 1,
                    CriticalOvertopping = 1
                };

            const double criticalOvertoppingRate = 0.001;

            for (int i = 0; i < npoints; i++)
            {
                xcoords[i] = 5*(i + 1);
                ycoords[i] = 3 + 2*(i + 1);
                roughness[i] = 1;
            }

            const double normal = 60.0; // degrees

            var wave = new Wave {WaterLevel = 5.5, Direction = 50, Height = 1, Steepness = 0.04};
            var load = new OvertoppingFortranAccess.OvertoppingLoadStruct
            {
                WaterLevel = wave.WaterLevel,
                Height = wave.Height,
                Period = wave.Period,
                Direction = wave.Direction
            };

            var result = OvertoppingFortranAccess.GetDischarge(load, normal, xcoords, ycoords, roughness, dikeHeight, modelFactors);
            var z = OvertoppingFortranAccess.GetZValue(criticalOvertoppingRate, modelFactors, result.Qo);

            //Assert.IsTrue(succes, errorMessage);
            Assert.AreEqual(z, zExpected1A, margin, "Z value from dllOvertopping.dll");

            wave.WaterLevel = 9.5; //overflow test
            var load2 = new OvertoppingFortranAccess.OvertoppingLoadStruct
            {
                WaterLevel = wave.WaterLevel,
                Height = wave.Height,
                Period = wave.Period,
                Direction = wave.Direction
            };
            result = OvertoppingFortranAccess.GetDischarge(load2, normal, xcoords, ycoords, roughness, dikeHeight, modelFactors);
            z = OvertoppingFortranAccess.GetZValue(criticalOvertoppingRate, modelFactors, result.Qo);

            Assert.AreEqual(z, zExpected2, margin, "Z value from dllOvertopping.dll; overflow test");

            wave.WaterLevel = 5.5;
            wave.Height = 0; // no waves test
            var load3 = new OvertoppingFortranAccess.OvertoppingLoadStruct
            {
                WaterLevel = wave.WaterLevel,
                Height = wave.Height,
                Period = wave.Period,
                Direction = wave.Direction
            };

            result = OvertoppingFortranAccess.GetDischarge(load3, normal, xcoords, ycoords, roughness, dikeHeight, modelFactors);
            z = OvertoppingFortranAccess.GetZValue(criticalOvertoppingRate, modelFactors, result.Qo);
            Assert.AreEqual(z, zExpected1B, margin, "Z value from dllOvertopping.dll; no waves test");

        }

    }
}