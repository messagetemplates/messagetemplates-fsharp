using MessageTemplates.Events;
using MessageTemplates.Parsing;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MessageTemplates
{
    /// <summary>
    /// The main user-facing API with methods for parsing, formatting, and
    /// capturing properties from message templates.
    /// </summary>
    public static class Template
    {
        public static MessageTemplate Parse(string templateMessage)
        {
            return new MessageTemplateParser().Parse(templateMessage);
        }

        public static IEnumerable<LogEventProperty> Capture(MessageTemplate template, params object[] args)
        {
            var binder = new MessageTemplates.Parameters.PropertyBinder(
                new MessageTemplates.Parameters.PropertyValueConverter(
                    10, Enumerable.Empty<Type>(), Enumerable.Empty<MessageTemplates.Core.IDestructuringPolicy>()));

            return binder.ConstructProperties(template, args);
        }

        public static void Format(
            IFormatProvider formatProvider,
            System.IO.TextWriter output,
            string templateMessage,
            params object[] args)
        {
            var template = Parse(templateMessage);
            var props = Capture(template, args);
            template.Render(props.ToDictionary40(x => x.Name, x => x.Value), output, formatProvider);
        }

        public static string Format(
            IFormatProvider formatProvider,
            string templateMessage,
            params object[] args)
        {
            var sw = new System.IO.StringWriter(formatProvider);
            Format(formatProvider, sw, templateMessage, args);
            sw.Flush();
            return sw.ToString();
        }

        public static string Format(
            string templateMessage,
            params object[] args)
        {
            return Format(System.Globalization.CultureInfo.InvariantCulture, templateMessage, args);
        }
    }
}
