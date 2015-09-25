using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MessageTemplates.PerfTests.Logging.LogProviders;

namespace MessageTemplates.PerfTests.Logging
{
    public static class Helper
    {
        public enum LibLogKind
        {
            Serilog,
            Log4Net,
        }

        public static ILogProvider ResolveLogProvider(LibLogKind kind)
        {
            switch (kind)
            {
                case LibLogKind.Serilog: return new SerilogLogProvider();
                case LibLogKind.Log4Net: return new Log4NetLogProvider();
                default:
                    throw new ArgumentOutOfRangeException(nameof(kind), kind, null);
            }
        }
    }
}
