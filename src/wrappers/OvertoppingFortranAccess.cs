using System;
using System.Runtime.InteropServices;
using System.Text;

namespace Deltares.Overtopping.Calculation
{
    public static class OvertoppingFortranAccess
    {
        private const int ErrorMessageLength = 255;

        public struct OvertoppingLoadStruct
        {
            public double WaterLevel;
            public double Height;
            public double Period;
            public double Direction;
        }

        public struct OvertoppingGeometryStruct
        {
            public double Normal;
            public int NPoints;
            public IntPtr XCoords;
            public IntPtr YCoords;
            public IntPtr Roughness;
        }

        public struct OvertoppingModelFactors
        {
            public double FactorDeterminationQnFn;
            public double FactorDeterminationQbFb;
            public double Frunup1;
            public double Frunup2;
            public double Frunup3;
            public double Fshallow;
            public double ComputedOvertopping;
            public double CriticalOvertopping;
        }

        public struct OvertoppingResult
        {
            public double Z2;
            public double Qo;
        }

        public static OvertoppingResult GetDischarge(OvertoppingLoadStruct wave, double normal, double[] xCoords, double[] zCoords, double[] roughness,
                                             double dikeHeight, OvertoppingModelFactors modelFactors)
        {

            var geometry = new OvertoppingGeometryStruct
                {
                    Normal = normal,
                    NPoints = xCoords.Length,
                    XCoords = Marshal.AllocHGlobal(Marshal.SizeOf(xCoords[0])*xCoords.Length),
                    YCoords = Marshal.AllocHGlobal(Marshal.SizeOf(zCoords[0])*zCoords.Length),
                    Roughness = Marshal.AllocHGlobal(Marshal.SizeOf(roughness[0])*roughness.Length)
                };
            Marshal.Copy(xCoords, 0, geometry.XCoords, xCoords.Length);
            Marshal.Copy(zCoords, 0, geometry.YCoords, zCoords.Length);
            Marshal.Copy(roughness, 0, geometry.Roughness, roughness.Length);

            var success = false;
            var errorMessage = new StringBuilder(ErrorMessageLength);
            OvertoppingResult result;
            result.Qo = double.NaN;
            result.Z2 = 0.0;
            calculateQo(ref wave, ref geometry, ref dikeHeight, ref modelFactors, ref result, ref success, errorMessage, errorMessage.Capacity);

            Marshal.FreeHGlobal(geometry.XCoords);
            Marshal.FreeHGlobal(geometry.YCoords);
            Marshal.FreeHGlobal(geometry.Roughness);

            if (!success) {throw new Exception(errorMessage.ToString());}
            return result;
        }

        public static double GetZValue(double criticalOvertoppingRate, OvertoppingModelFactors modelFactors, double qo)
        {
            var success = false;
            var errorMessage = new StringBuilder(ErrorMessageLength);
            var z = double.NaN;

            calcZValue(ref criticalOvertoppingRate, ref modelFactors, ref qo, ref z, ref success, errorMessage, errorMessage.Capacity);
            if (!success) {throw new Exception(errorMessage.ToString());}
            return z;
        }

        [DllImport("dllOvertopping.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void calcZValue(ref double criticalOvertoppingRate, ref OvertoppingModelFactors modelFactors, ref double qo, ref double z, ref bool success, StringBuilder message, int stringLength);

        [DllImport("dllOvertopping.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void calculateQo(ref OvertoppingLoadStruct load, ref OvertoppingGeometryStruct geometry, ref double dikeHeight, ref OvertoppingModelFactors modelFactors, ref OvertoppingResult result, ref bool success, StringBuilder message, int stringLength);
    }
}