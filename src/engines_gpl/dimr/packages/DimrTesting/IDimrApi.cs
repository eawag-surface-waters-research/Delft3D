using System;
using DimrTesting;
using log4net.Core;
using BasicModelInterface;
using IBasicModelInterface = BasicModelInterface.IBasicModelInterface;
using Level = BasicModelInterface.Level;

namespace Dimr
{
    public interface IDimrApi : IDisposable, IBasicModelInterface
    {
        string KernelDirs { get; set; }
        DateTime DimrRefDate { get; set; }
        void set_feedback_logger();
        string[] Messages { get; }
        void ProcessMessages();
        void SetValuesDouble(string variable, double[] values);
        void SetValuesInt(string variable, int[] values);
        void SetLoggingLevel(string logType, Level level);
    }
}