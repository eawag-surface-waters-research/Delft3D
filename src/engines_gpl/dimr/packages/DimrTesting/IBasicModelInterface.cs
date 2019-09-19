using System.Collections.Generic;
using System;

namespace DimrTesting
{
    public interface IBasicModelInterface
    {
        DateTime StartTime { get; }

        DateTime StopTime { get; }

        DateTime CurrentTime { get; }

        TimeSpan TimeStep { get; }

        /// <summary>
        /// Gets names of all available variables.
        /// </summary>
        string[] VariableNames { get; }

        /// <summary>
        /// Injects logger into the model so that it can log messages. A callback method.
        /// </summary>


        /// <summary>
        /// Initializes model using a given configuration file..
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        int Initialize(string path);

        int Update(double dt = -1);

        int Finish();

        /// <summary>
        /// Gets variable values.
        /// </summary>
        /// <param name="variable"></param>
        /// <returns></returns>
        Array GetValues(string variable);

        //        /// <summary>
        //        /// Gets variable values by index (flattened nD index).
        //        /// </summary>
        //        /// <param name="variable"></param>
        //        /// <param name="index"></param>
        //        /// <returns></returns>
        //        Array GetValues(string variable, int[] index);

        //        /// <summary>
        //        /// Gets variable values by slice (flattened nD array of Rank x NumberOfValues).
        //        /// </summary>
        //        /// <param name="variable"></param>
        //        /// <param name="start"></param>
        //        /// <param name="count"></param>
        //        /// <returns></returns>
        //        Array GetValues(string variable, int[] start, int[] count);

        /// <summary>
        /// Sets variable values.
        /// </summary>
        /// <param name="variable"></param>
        /// <param name="values"></param>
        void SetValues(string variable, Array values);

        //        /// <summary>
        //        /// Sets variable values by slice (start + count for every dimension).
        //        /// </summary>
        //        /// <param name="variable"></param>
        //        /// <param name="start"></param>
        //        /// <param name="count"></param>
        //        /// <param name="values"></param>
        //        void SetValues(string variable, int[] start, int[] count, Array values);

        //        /// <summary>
        //        /// Sets variable values by indices (flattened nD array of Rank x NumberOfValues).
        //        /// </summary>
        //        /// <param name="variable"></param>
        //        /// <param name="index"></param>
        //        /// <param name="values"></param>
        //        void SetValues(string variable, int[] index, Array values);


    }
}
