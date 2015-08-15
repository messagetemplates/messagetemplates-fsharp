using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Diagnostics;
using System.Linq;

namespace MessageTemplates.PerfTests
{
    [TestClass]
    public class PerfTests
    {
        const int TEST_ITERATIONS = 50000;

        static void Main(string[] args)
        {
            //var template = "Hello, {@name}, how's it {@going}?";
            //var formatArgs = new object[] { "adam", new Chair() };
            //var tw = new System.IO.StringWriter();

            //var sw = new Stopwatch();
            //sw.Start();
            //for (var i = 0; i < 1000; i++)
            //{
            //    FsMessageTemplates.MessageTemplates.fprintsm(tw, template, formatArgs);
            //    tw.WriteLine();
            //}
            //sw.Stop();
            //System.IO.File.WriteAllText("output_fs.txt", tw.ToString());
            //Console.WriteLine(sw.Elapsed);

            //tw.GetStringBuilder().Clear();

            //sw.Restart();
            //for (var i = 0; i < 200000; i++)
            //{
            //    MessageTemplate.Format(tw.FormatProvider, tw, template, formatArgs);
            //    tw.WriteLine();
            //}
            //sw.Stop();
            //System.IO.File.WriteAllText("output_cs.txt", tw.ToString());
            //Console.WriteLine(sw.Elapsed);

            var t = new PerfTests();
            Action<Action> test = (action) =>
            {
                var name = action.Method.Name;
                action(); // warmup
                GC.Collect(3, GCCollectionMode.Forced, blocking: true);
                var sw = new Stopwatch();
                sw.Start();
                action();
                sw.Stop();
                Console.WriteLine("{0}\t{1}", sw.Elapsed, name);
            };

            test(t.CSharpParseNamed);
            test(t.FSharpParseNamed);
            Console.WriteLine();
            test(t.CSharpParsePositional);
            test(t.FSharpParsePositional);
            Console.WriteLine();
            test(t.CSharpParseAndFormatNamed);
            test(t.FSharpParseAndFormatNamed);
            Console.WriteLine();
            test(t.CSharpParseAndFormatPositional);
            test(t.FSharpParseAndFormatPositional);
            Console.WriteLine();
            test(t.CSharpFormatPositional);
            test(t.FSharpFormatPositional);
        }

        readonly string[] NAMED_TEMPLATES = new[] {
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

        readonly string[] POSITIONAL_TEMPLATES = new[] {
            "Hello, {namsd,fgsdfg{{adam}}}, how's it {1,-10:blah:bhal}? {2:0,0} {3:0,0} {4:0,0} {5:0,0} {6:0,0}",
            "Welcome, customer #{1,-10}, pleasure to see you",
            "Welcome, customer #{3,-10:000000}, pleasure to see you",
            "Welcome, customer #{5,10}, pleasure to see you",
            "Welcome, customer #{2,10:000000}, pleasure to see you",
            "Welcome, customer #{4,10:0,0}, pleasure to see you",
            "Welcome, customer #{6:0,0}, pleasure to see you",
            "Welcome, customer #{2,-10}, pleasure to see you",
            "Welcome, customer #{1,-10:000000}, pleasure to see you",
            "Welcome, customer #{3,10}, pleasure to see you",
            "Welcome, customer #{5,10:000000}, pleasure to see you",
            "Welcome, customer #{6,10:0,0}, pleasure to see you",
            "Welcome, customer #{9:0,0}, pleasure to see you",
        };

        readonly object[][] ARGS = new[]
        {
            new object[] { 1, 2, 3, 4, 5, 6 },
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

        private readonly IFormatProvider formatProvider = System.Globalization.CultureInfo.InvariantCulture;

        [TestMethod]
        public void CSharpParseNamed()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < NAMED_TEMPLATES.Length; x++)
                {
                    MessageTemplates.MessageTemplate.Parse(NAMED_TEMPLATES[x]);
                }
            }
        }

        [TestMethod]
        public void CSharpParsePositional()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < POSITIONAL_TEMPLATES.Length; x++)
                {
                    MessageTemplates.MessageTemplate.Parse(POSITIONAL_TEMPLATES[x]);
                }
            }
        }

        [TestMethod]
        public void FSharpParseNamed()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < NAMED_TEMPLATES.Length; x++)
                {
                    FsMessageTemplates.MessageTemplates.parse(NAMED_TEMPLATES[x]);
                }
            }
        }

        [TestMethod]
        public void FSharpParsePositional()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < POSITIONAL_TEMPLATES.Length; x++)
                {
                    FsMessageTemplates.MessageTemplates.parse(POSITIONAL_TEMPLATES[x]);
                }
            }
        }

        [TestMethod]
        public void CSharpParseAndFormatNamed()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < NAMED_TEMPLATES.Length; x++)
                {
                    MessageTemplate.Format(formatProvider, NAMED_TEMPLATES[x], ARGS[x]);
                }
            }
        }

        [TestMethod]
        public void CSharpParseAndFormatPositional()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < POSITIONAL_TEMPLATES.Length; x++)
                {
                    MessageTemplate.Format(formatProvider, POSITIONAL_TEMPLATES[x], ARGS[x]);
                }
            }
        }

        [TestMethod]
        public void FSharpParseAndFormatPositional()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < POSITIONAL_TEMPLATES.Length; x++)
                {
                    var mt = FsMessageTemplates.MessageTemplates.parse(POSITIONAL_TEMPLATES[x]);
                    FsMessageTemplates.MessageTemplates.format(mt, ARGS[x]);
                }
            }
        }

        [TestMethod]
        public void FSharpParseAndFormatNamed()
        {
            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < NAMED_TEMPLATES.Length; x++)
                {
                    var mt = FsMessageTemplates.MessageTemplates.parse(NAMED_TEMPLATES[x]);
                    FsMessageTemplates.MessageTemplates.sprintm(mt, formatProvider, ARGS[x]);
                }
            }
        }

        [TestMethod]
        public void CSharpFormatPositional()
        {
            var templates = POSITIONAL_TEMPLATES.Select(MessageTemplate.Parse).ToArray();
            var tw = new System.IO.StringWriter();

            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < templates.Length; x++)
                {
                    templates[x].Format(tw.FormatProvider, tw, ARGS[x]);
                    tw.GetStringBuilder().Clear();
                }
            }
        }

        [TestMethod]
        public void FSharpFormatPositional()
        {
            var templates = POSITIONAL_TEMPLATES.Select(FsMessageTemplates.MessageTemplates.parse).ToArray();
            var tw = new System.IO.StringWriter();

            for (var i = 0; i < TEST_ITERATIONS; i++)
            {
                for (var x = 0; x < templates.Length; x++)
                {
                    FsMessageTemplates.MessageTemplates
                        .fprintm(templates[x], tw, ARGS[x]);
                    tw.GetStringBuilder().Clear();
                }
            }
        }

    }
}
