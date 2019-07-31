#include "DimrTests.h"
#include "dimr.h"



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

	TEST_METHOD(initializeWithFileNameAndFileTest)
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
	}
	
};
