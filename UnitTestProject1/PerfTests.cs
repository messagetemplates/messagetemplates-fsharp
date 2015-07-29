using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace MessageTemplates.PerfTests
{
    [TestClass]
    public class PerfTests
    {
        string[] TEMPLATES = new[] {
            "Hello, {namsd,fgsdfg{{adam}}}, how's it {going,-10:blah:bhal}? {going:0,0} {going:0,0} {going:0,0} {going:0,0} {going:0,0}",
            "Welcome, customer #{CustomerId,-10}, pleasure to see you",
            "Welcome, customer #{CustomerId,-10:000000}, pleasure to see you",
            "Welcome, customer #{CustomerId,10}, pleasure to see you",
            "Welcome, customer #{CustomerId,10:000000}, pleasure to see you",
            "Welcome, customer #{CustomerId,10:0,0}, pleasure to see you",
            "Welcome, customer #{CustomerId:0,0}, pleasure to see you",
            "Welcome, customer #{CustomerId,-10}, pleasure to see you",
            "Welcome, customer #{CustomerId,-10:000000}, pleasure to see you",
            "Welcome, customer #{CustomerId,10}, pleasure to see you",
            "Welcome, customer #{CustomerId,10:000000}, pleasure to see you",
            "Welcome, customer #{CustomerId,10:0,0}, pleasure to see you",
            "Welcome, customer #{CustomerId:0,0}, pleasure to see you",
        };

        const int TEST_ITERATIONS = 30000;

        [TestMethod]
        public void CSharpPerformanceIsGoodFor30kParseRounds()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < TEMPLATES.Length; x++)
                {
                    MessageTemplates.MessageTemplate.Parse(TEMPLATES[x]);
                }
            }
        }

        [TestMethod]
        public void FSharpPerformanceIsGoodFor30kParseRounds()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < TEMPLATES.Length; x++)
                {
                    FsMessageTemplates.MessageTemplates.parse(TEMPLATES[x]);
                }
            }
        }

    }
}
