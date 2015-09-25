namespace MessageTemplates.PerfTests
{
    class Chair
    {
        // ReSharper disable UnusedMember.Local
        public string Back { get { return "straight"; } }
        public int[] Legs { get { return new[] { 1, 2, 3, 4 }; } }
        // ReSharper restore UnusedMember.Local
        public override string ToString()
        {
            return "a chair";
        }
    }
}