using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DimrTesting
{
    class Program
    {
        static void Main(string[] args)
        {
            IBasicModelInterface model = new Sobek3ModelWrapper();
            model.Initialize(@"dimr.xml");
            model.Update(600.0);
            model.Finish();
        }
    }
}
