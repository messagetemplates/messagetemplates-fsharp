using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace MessageTemplates.PerfTests
{
    [TestClass]
    public class PerfTests
    {
        readonly string[] TEMPLATES = new[] {
            "Hello, {namsd,fgsdfg{{adam}}}, how's it {going1,-10:blah:bhal}? {going2:0,0} {going3:0,0} {going4:0,0} {going5:0,0} {going6:0,0}",
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

        readonly object[][] ARGS = new[]
        {
            new object[] { 1, 2, 3, 4, 5, 6 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
            new object[] { 1 },
        };

        const int TEST_ITERATIONS = 20000;
        private readonly IFormatProvider formatProvider = System.Globalization.CultureInfo.InvariantCulture;

        [TestMethod]
        public void CSharpPerformanceIsGoodFor3kParseRounds()
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
        public void FSharpPerformanceIsGoodFor3kParseRounds()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < TEMPLATES.Length; x++)
                {
                    FsMessageTemplates.MessageTemplates.parse(TEMPLATES[x]);
                }
            }
        }

        [TestMethod]
        public void CSharpPerformanceIsGoodFor3kParseThenFormatRounds()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < TEMPLATES.Length; x++)
                {
                    MessageTemplate.Format(formatProvider, TEMPLATES[x], ARGS[x]);
                }
            }
        }

        [TestMethod]
        public void FSharpPerformanceIsGoodFor3kParseThenFormatRounds()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < TEMPLATES.Length; x++)
                {
                    var mt = FsMessageTemplates.MessageTemplates.parse(TEMPLATES[x]);
                    FsMessageTemplates.MessageTemplates.format(formatProvider, mt, ARGS[x]);
                }
            }
        }

    }
}
