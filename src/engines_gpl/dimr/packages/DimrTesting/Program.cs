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
            Array bufArr;
            IBasicModelInterface model = new Sobek3ModelWrapper();
            model.Initialize(@"dimr.xml");
            model.Update(60.0);
            //
            bufArr = model.GetValues("water flow 1d/observations/HEESBN/water_level");
            model.SetValues("real-time control/input_HEESBN_Water level (op)", bufArr);
            //
            bufArr = model.GetValues("real-time control/output_Kromme Nolkering_Gate lower edge level (s)");
            model.SetValues("water flow 1d/weirs/Kromme Nolkering/structure_gate_lower_edge_level", bufArr);
            //
            model.Finish();
        }
    }
}
