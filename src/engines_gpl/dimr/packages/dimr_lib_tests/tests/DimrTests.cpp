#include "DimrTests.h"
#include "dimr.h"
#include <memory>
#include <direct.h>
#include <functional>


TEST_CLASS(DimrGlobalTests)
{

public:
	
    TEST_METHOD(initializeWithoutFileNameTest)
	{
		Assert::AreEqual(initialize(""), (int)Exception::ERR_OS);
	}
	
	TEST_METHOD(initializeWithFileNameTest)
	{
		Assert::AreEqual(initialize("dimr.xml"), (int)Exception::ERR_INVALID_INPUT);
	}

	TEST_METHOD(initializeWithEmptyFileNameAndFileTest)
	{
		std::ofstream file{ "dimr.xml" };

		Assert::AreEqual(initialize("dimr.xml"), (int)Exception::ERR_INVALID_INPUT);
	}
};


TEST_CLASS(DimrTests)
{
public:
	TEST_METHOD(ConstructDimr)
	{
		Dimr * dimr = new Dimr();
		Assert::AreEqual((int)dimr->logLevel, (int)WARNING);
		Assert::AreEqual((int)dimr->feedbackLevel, (int)INFO);
        Assert::AreEqual(dimr->ready, false);
        Assert::AreEqual(dimr->exePath, NULL);
        Assert::AreEqual((int)dimr->config, NULL);
        Assert::AreEqual(dimr->mainArgs, NULL);
        Assert::AreEqual(dimr->slaveArg, NULL);
        Assert::AreEqual((int)dimr->control, NULL);
        Assert::AreEqual((int)dimr->componentsList.numComponents, 0);
        Assert::AreEqual((int)dimr->couplersList.numCouplers , 0);
        Assert::AreEqual(dimr->use_mpi ,false);
        Assert::AreEqual(dimr->my_rank ,0);
        Assert::AreEqual(dimr->numranks ,1);
        Assert::AreEqual(dimr->configfile ,NULL);
        Assert::AreEqual(dimr->done ,false);
        Assert::AreEqual(dimr->redirectFile, "dimr_redirected.log");
        //Cannot delete properly yet
        //delete dimr;
	}


    TEST_METHOD(scanConfigIsEmptyTest)
    {
        Dimr* dimr = new Dimr();
        //dimr->config = new XmlTree();
        auto func = [dimr] {dimr->scanConfigFile(); };
        Assert::ExpectException<Exception>(func);
        //Cannot delete properly yet
        //delete dimr;
    }
    
    TEST_METHOD(scanConfigIsNotEmptyButFileVersionIsNullTest)
    {
        Dimr* dimr = new Dimr();
        //dimr->config = new XmlTree();
        auto func = [dimr] {dimr->scanConfigFile(); };
        Assert::ExpectException<Exception>(func);
        //Cannot delete properly yet
        //delete dimr;
    }
    
    TEST_METHOD(WhenInvalidLibIsUsedIn_ConnectLibsThrowsAnException)
    {
        Dimr* dimr = new Dimr();

        // sets component
        dimr_component component;
        component.onThisRank = true;
        component.library = "invalidLib";

        // set the componentsList
        auto componentsList = dimr_components();
        componentsList.components = &component;
        componentsList.numComponents = 1;

        dimr->componentsList = componentsList;
        auto func = [&] {dimr->connectLibs(); };
        Assert::ExpectException<Exception>(func);
        //Cannot delete properly yet
	    //delete dimr;
    }

    TEST_METHOD(WhenInvalidLibPathIsUsedIn_ConnectLibsThrowsAnException)
    {
        Dimr* dimr = new Dimr();

        // sets component
        dimr_component component;
        component.onThisRank = true;
        component.library = "\dimr_testcomponent.dll";

        // set the componentsList
        auto componentsList = dimr_components();
        componentsList.components = &component;
        componentsList.numComponents = 1;

        dimr->componentsList = componentsList;
        auto func = [&] {dimr->connectLibs(); };
        Assert::ExpectException<Exception>(func);
        //Cannot delete properly yet
        //delete dimr;
    }

    //cannot go further, need a valid dll to test the rest of the connectLibs

    TEST_METHOD(WhenRunParallelInitIsUsedWithValidMasterComponent_EverythingIsFine)
    {
        Dimr* dimr = new Dimr();
        Clock clock;
        dimr->clock = &clock;

        // Set up
        dimr_control_block cb;
        cb.numSubBlocks = 1;

        dimr_control_block controlBlock;
        dimr_unit dimrUnit;
        dimr_component masterComponent;
        masterComponent.name = "masterComponent";
        char cCurrentPath[FILENAME_MAX];
        _getcwd(cCurrentPath, sizeof(cCurrentPath));
        masterComponent.workingDir = cCurrentPath;
        masterComponent.inputFile = "";
        masterComponent.dllInitialize = [](const char * c) { return 0; };
        masterComponent.dllGetStartTime = [](double * time) { };
        masterComponent.dllGetEndTime = [](double * time) {};
        masterComponent.dllGetTimeStep = [](double * time) {};
        masterComponent.dllGetCurrentTime = [](double * time) {};

        dimrUnit.component = &masterComponent;
        controlBlock.unit = dimrUnit;

        controlBlock.type = CT_START;
        cb.subBlocks = &controlBlock;
        cb.masterSubBlockId = -1;

        // set the componentsList
        dimr->runParallelInit(&cb);
        //Cannot delete properly yet
        //delete dimr;
    }

};

/*
void           scanConfigFile(void);
void           connectLibs(void);
void           printComponentVersionStrings(Level);
void           freeLibs(void);
void           processWaitFile(void);
void           runControlBlock(dimr_control_block *, double, int);
void           runParallelInit(dimr_control_block *);
void           runParallelFinish(dimr_control_block *);
void           timersInit(void);
void           timerStart(dimr_component *);
void           timerEnd(dimr_component *);
void           timersFinish(void);
void           receive(const char *, int, BMI_SETVAR, BMI_GETVAR, double *, int *, int, int, const void *);
void           getAddress(const char * name, int compType, BMI_GETVAR dllGetVar, double ** sourceVarPtr, int * processes, int nProc, double * transfer);
double *       send(const char * name, int compType, double* sourceVarPtr, int* processes, int nProc, double* transfer);
*/

