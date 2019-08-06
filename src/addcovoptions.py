from lxml import etree, objectify
import re
import sys
import os.path
import copy

covcfgname = "CodeCoverage|x64"    # name of the new configuration for coverage testing
targetconfig = 'Release|x64'       # name of the configyration from which to copy

def add_cov(inproj, outproj, CoveragePath):
    global covcfgname, targetconfig
    coverage_options = " /Qcov-gen /Qcov-dir  "+ CoveragePath
    my_parser = etree.XMLParser(remove_blank_text=True)
    xmltree = etree.parse(inproj,my_parser)
    root=xmltree.getroot()
    for child in root:
        if (child.tag=='Configurations'):
            configs = child
            # copy the release 64bit configuration into the new release 64bit code coverage configuration
            for child in configs:
                if (child.tag=='Configuration'):
                    config = child
                    if config.attrib['Name']==targetconfig:
                        newcfg = copy.deepcopy(config)
                        newcfg.attrib['Name'] = covcfgname
                        for child in newcfg:
                            if (child.tag=='Tool' and child.attrib['Name']=='VFFortranCompilerTool'):
                                addopts = ""
                                if 'AdditionalOptions' in child.attrib:
                                    addopts = child.attrib['AdditionalOptions']
                                addopts = addopts + " " +coverage_options
                                child.attrib['AdditionalOptions'] = addopts
                        configs.append(newcfg)
    xmltree.write(outproj, pretty_print=True, xml_declaration=True,   encoding="utf-8")
    return xmltree

def iter_projects(solution,covpath):
    fsln = open(solution,"r")
    slntxt = fsln.readlines()
    fsln.close()
    slndir = os.path.join(os.getcwd(),os.path.dirname(solution))
    projguidlist = []
    for line in slntxt:
        m = re.search("Project\(.*\)\s*=\s*\"(.+)\"\s*,\s*\"(.+)\"\s*,\s*\"(.+)\"",line)
        if m:
            project_name = m.group(1)
            project_path = m.group(2)	# always relative wrt solution dir
            project_guid = m.group(3)
            projguidlist.append(project_guid)
            project_full_path = os.path.join(slndir,project_path) 
            if re.match(".*proj$",project_full_path):
                inproj = project_full_path
                outproj = project_full_path
                resultpath = os.path.join(slndir,covpath)
                if not(os.path.exists(resultpath)):
                    os.makedirs(resultpath)
                add_cov(inproj, outproj, resultpath)
                # print (inproj + ' ---> ' + outproj)


    fsln_out = open("_coverage".join(os.path.splitext(solution)),"w")
    for line in slntxt:
        fsln_out.write(line)
        if 'preSolution' in line:
            fsln_out.write("\t\t%s = %s\n"%(covcfgname,covcfgname))            
        if 'postSolution' in line:
            for projguid in projguidlist:
                fsln_out.write("\t\t{%s}.%s.ActiveCfg = %s\n"%(projguid,covcfgname,covcfgname))
                fsln_out.write("\t\t{%s}.%s.Build.0 = %s\n"%(projguid,covcfgname,covcfgname))
    fsln_out.close()


fsln = sys.argv[1]
slndir = os.path.dirname(fsln)
iter_projects(fsln,'coverage_results')

