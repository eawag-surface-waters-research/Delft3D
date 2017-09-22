import os
from SharpMap import Map
from DelftTools.Shell.Gui import MapLayerProviderHelper
from DeltaShell.Plugins.FMSuite.FlowFM.IO import FMMapFileFunctionStore

# import all FM output map files
dir = r'D:\src\ds\trunk\additional\unstruc\test_data\san_francisco\parallel\DFM_OUTPUT_r07e_bay'
files = [os.path.join(dir, f) for f in os.listdir(dir) if f.startswith('r07e_bay_00') and f.endswith('map.nc')]
stores = [FMMapFileFunctionStore(f) for f in files]

# create group layers for every imported map file
layer_provider = next(p.MapLayerProvider for p in Gui.Plugins if p.Name == r'FM 2D/3D Suite (UI)')
map = Map()
for f, s in zip(files, stores):
    layer = MapLayerProviderHelper.CreateLayersRecursive(s, None, [layer_provider])
    layer.Name = os.path.basename(f)
    layer.Layers[1].Visible = True
    map.Layers.Add(layer)

CurrentProject.RootFolder.Add(map)

# make sure all layers use the same min/max
theme_min = min(l.Layers[1].Theme.Min for l in map.Layers)
theme_max = max(l.Layers[1].Theme.Max for l in map.Layers)
for l in map.Layers:
    l.Layers[1].Theme.SetMinMax(theme_min, theme_max)

