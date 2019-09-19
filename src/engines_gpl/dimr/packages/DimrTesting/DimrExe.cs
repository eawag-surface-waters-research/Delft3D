using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using log4net;
using BasicModelInterface;


namespace Dimr
{
    public static class DimrApiDataSet
    {
#if DEBUG
        public const string exeName = @"dimr.exe";
        //public const string exeName = @"/../../../../../bin/x64/Debug/dimr.exe";
#else
        public const string exeName = @"/../../../../../bin/x64/Release/dimr.exe";

#endif
        public const string DIMR_DLL_NAME = "dimr_dll.dll";
        public const string DIMR_EXE_NAME = "dimr.exe";
        private const string DIMR_FOLDER_NAME = "dimr";
        private const string STANDARD_BINFOLDER_NAME = "bin";

        public static string KernelsDirectory
        {
            get
            {
                return Path.Combine(Path.GetDirectoryName(typeof(IDimrApi).Assembly.Location), "kernels");
            }
        }

        public static string DimrExePath
        {
            get { return Path.GetDirectoryName(typeof(IDimrApi).Assembly.Location); }
            //get { return Path.Combine(KernelsDirectory, "x64", DIMR_FOLDER_NAME, STANDARD_BINFOLDER_NAME); }
        }

    }

    public class DimrExe : IDimrApi
    {
        private readonly bool useMessagesBuffering;
        private static readonly ILog Log = LogManager.GetLogger(typeof(DimrExe));
        private string configFile;
        
        private int processId = -1;
        private readonly string[] messages;

        public DimrExe(bool useMessagesBuffering)
        {
            this.useMessagesBuffering = useMessagesBuffering;
            messages = new []{ string.Empty };
        }

        private void Run(string xmlFile)
        {
            var previousDir = Environment.CurrentDirectory;

            try
            {
                Environment.CurrentDirectory = Path.GetDirectoryName(xmlFile);
                Log.Info(string.Format("Running dimr in : {0}", Environment.CurrentDirectory));
                
                var dimrProcInfo = new ProcessStartInfo
                    {
                        UseShellExecute = false,
                        RedirectStandardError = true,
                        RedirectStandardOutput = true,
                        WindowStyle = ProcessWindowStyle.Hidden,
                        CreateNoWindow = true,
                        ErrorDialog = false,
                        FileName = Path.Combine(DimrApiDataSet.DimrExePath, DimrApiDataSet.exeName),
                        Arguments = xmlFile,
                        WorkingDirectory = Path.GetDirectoryName(xmlFile)
                };


                dimrProcInfo.EnvironmentVariables["PATH"] = KernelDirs + ";" +
                                                            Path.Combine(Environment.CurrentDirectory, "../../../kernels/x64/") +
                                                            dimrProcInfo.EnvironmentVariables["PATH"];
                dimrProcInfo.EnvironmentVariables["D3D_HOME"] = Path.Combine(Environment.CurrentDirectory,"../../../kernels/");
                dimrProcInfo.EnvironmentVariables["PROC_DEF_DIR"] = Path.Combine(Environment.CurrentDirectory,"../../../kernels/x64/dwaq/default/");
                dimrProcInfo.EnvironmentVariables["ARCH"] = "win64";
                dimrProcInfo.EnvironmentVariables["OMP_NUM_THREADS"] = "1";
                dimrProcInfo.EnvironmentVariables["OMP_NUM_THREADS_SWAN"] = "1";



                var infoMessages = new List<string>();
                var warnMessages = new List<string>();

                using (var process = new Process { StartInfo = dimrProcInfo })
                {
                    process.OutputDataReceived += (o, e) =>
                        {
                            if (e.Data == null) return;
                            if (useMessagesBuffering)
                            {
                                infoMessages.Add(e.Data);
                            }
                            else
                            {
                                Log.Info(e.Data);
                            }
                        };
                    process.ErrorDataReceived += (o, e) =>
                    {
                        if (e.Data == null) return;

                        var message = string.Format("Error occurred while running Dimr.exe: {0}", e.Data);
                        if (useMessagesBuffering)
                        {
                            warnMessages.Add(message);
                        }
                        else
                        {
                            Log.Warn(message);
                        }
                    };

                    process.Start();
                    process.BeginOutputReadLine();
                    process.BeginErrorReadLine();
                    processId = process.Id;
                    process.WaitForExit();
                    process.WaitForExit();

                    if (useMessagesBuffering)
                    {
                        infoMessages.ForEach(m => Log.Info(m));
                        warnMessages.ForEach(m => Log.Warn(m));
                    }

                    if (process.ExitCode > 0)
                    {
                        throw new Exception("dimr returned error code " + process.ExitCode + Environment.NewLine + "Error: " +Environment.NewLine + string.Join(Environment.NewLine, infoMessages) ) ;
                    }
                }
            }
            finally
            {
                processId = -1;
                Environment.CurrentDirectory = previousDir;
            }
        }

        public void Dispose()
        {
            //euhm..
            if (processId != -1)
            {
                Log.Info("Canceling dimr run");
                Process.GetProcessById(processId).Kill();
                processId = -1;
            }
        }

        public string KernelDirs { get; set; }
        public DateTime DimrRefDate { get; set; }

        public void set_feedback_logger()
        {
            
        }

        public int Initialize(string xmlFile)
        {
            configFile = xmlFile;
            if (!File.Exists(configFile))
            {
                //hmm call exporter or something...
            }
            StartTime = StopTime = CurrentTime = DimrRefDate;
            return 0;
        }

        public int Update(double step)
        {
            Run(configFile);
            
            return 0;
        }

        public int Finish()
        {
            // throw everything away? set output into projects? etc..
            return 0;
        }

        public int[] GetShape(string variable)
        {
            return new int[] {};
        }

        public Array GetValues(string variable)
        {
            return null;
        }

        public Array GetValues(string variable, int[] index)
        {
            return null;
        }

        public Array GetValues(string variable, int[] start, int[] count)
        {
            return null;
        }

        public void SetValues(string variable, Array values)
        {
        }

        public void SetValues(string variable, int[] start, int[] count, Array values)
        {
        }

        public void SetValues(string variable, int[] index, Array values)
        {
        }

        public DateTime StartTime { get; private set; }
        public DateTime StopTime { get; private set; }
        public DateTime CurrentTime { get; private set; }
        public TimeSpan TimeStep { get; private set; }
        public string[] VariableNames { get; private set; }
        public Logger Logger { get; set; }

        public string[] Messages
        {
            get { return messages; }
        }

        public void ProcessMessages()
        {
            //nope..
        }

        public void SetValuesDouble(string variable, double[] values)
        {
        }

        public void SetValuesInt(string variable, int[] values)
        {
        }

        public void SetLoggingLevel(string logType, Level level)
        {
        }
    }
}