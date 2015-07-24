using MessageTemplates.Events;
using MessageTemplates.Parsing;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;

namespace MessageTemplates
{
    /// <summary>
    /// The main user-facing API with methods for parsing, formatting, and
    /// capturing properties from message templates.
    /// </summary>
    public static class Template
    {
        /// <summary>
        /// Parses a message template (e.g. "hello, {name}") into a 
        /// <see cref="MessageTemplate"/> structure.
        /// </summary>
        /// <param name="templateMessage">A message template (e.g. "hello, {name}")</param>
        /// <returns>The parsed message template.</returns>
        public static MessageTemplate Parse(string templateMessage)
        {
            return new MessageTemplateParser().Parse(templateMessage);
        }

        /// <summary>
        /// Captures properties from the given message template and 
        /// provided values.
        /// </summary>
        public static IEnumerable<LogEventProperty> Capture(
            MessageTemplate template, params object[] values)
        {
            var binder = new Parameters.PropertyBinder(
                new Parameters.PropertyValueConverter(
                    10,
                    Enumerable.Empty<Type>(),
                    Enumerable.Empty<Core.IDestructuringPolicy>()));

            return binder.ConstructProperties(template, values);
        }

        /// <summary>
        /// Formats the message template as a string, written into the text
        /// writer.
        /// </summary>
        public static void Format(
            IFormatProvider formatProvider,
            TextWriter output,
            string templateMessage,
            params object[] values)
        {
            var template = Parse(templateMessage);
            var props = Capture(template, values);
            template.Render(props.ToDictionary40(x => x.Name, x => x.Value), output, formatProvider);
        }

        /// <summary>
        /// Formats the message template as a string using the provided
        /// format provider and values.
        /// </summary>
        public static string Format(
            IFormatProvider formatProvider,
            string templateMessage,
            params object[] values)
        {
            var sw = new StringWriter(formatProvider);
            Format(formatProvider, sw, templateMessage, values);
            sw.Flush();
            return sw.ToString();
        }

        /// <summary>
        /// Formats the message template as a string using the provided values.
        /// </summary>
        public static string Format(
            string templateMessage,
            params object[] values)
        {
            return Format(CultureInfo.InvariantCulture, templateMessage, values);
        }
    }
}
