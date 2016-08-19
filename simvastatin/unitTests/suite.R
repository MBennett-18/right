library(RUnit)

testSuite <- defineTestSuite("simvastatin",
                             dirs = "simvastatin/unitTests",
                             testFileRegexp = "^test.+\\.R",
                             testFuncRegexp = "^test.+")

testResult <- runTestSuite(testSuite)

printTextProtocol(testResult)