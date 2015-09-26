(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/MessageTemplates"
#r "System.IO"

(**
Introducing Message Templates
=============================

*)
#r "MessageTemplates"
open MessageTemplates

MessageTemplate.Format("test {this}", "success")

(**
Some more info
*)
