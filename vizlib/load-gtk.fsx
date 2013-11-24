
#r "atk-sharp.dll"
#r "gdk-sharp.dll"
#r "gtk-sharp.dll"
#r "glib-sharp.dll"

(*
let f = new System.Windows.Forms.Form(Visible=true,TopMost=true)

f.OwnedForms
System.Windows.Forms.Application.RenderWithVisualStyles 
System.Windows.Forms.Application.SafeTopLevelCaptionFormat
System.Windows.Forms.Application.MessageLoop
*)

Gtk.Application.Init()

let win = new Gtk.Window("MainWindow")
    
win.Show()
win.Title <- "hello again"

let tb = new Gtk.TextView()
win.Add tb
tb.Show()
tb.Buffer.InsertAtCursor ("hello world!!!\n")


