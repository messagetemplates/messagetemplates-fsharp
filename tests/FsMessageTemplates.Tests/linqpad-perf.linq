<Query Kind="Statements">
  <Reference Relative="..\FsMessageTemplates\bin\Release\FsMessageTemplates.dll">C:\repo\MessageTemplates\FsMessageTemplates\bin\Release\FsMessageTemplates.dll</Reference>
  <NuGetReference>Serilog</NuGetReference>
  <Namespace>Serilog.Parsing</Namespace>
</Query>

Serilog.Debugging.SelfLog.Out = LINQPad.Util.SqlOutputWriter;
const int TEST_COUNT = 1000000;
const string TEST_TEMPL = "Well, {hello:-100,hh:mm}";

var items = new object[TEST_COUNT * 2];

var p = new Serilog.Parsing.MessageTemplateParser();
var sw = new System.Diagnostics.Stopwatch();

sw.Start();
for (var i = 0; i < TEST_COUNT; i++)
{
	items[i] = FsMessageTemplates.MessageTemplates.parse(TEST_TEMPL);
}
sw.Dump("F# parse");

sw.Restart();
for (var i = 0; i < TEST_COUNT; i++)
{
	items[TEST_COUNT - 1 + i] = p.Parse(TEST_TEMPL);
}
sw.Dump("C# parse");