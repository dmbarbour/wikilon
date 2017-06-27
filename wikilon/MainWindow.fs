namespace wikilon

    module MainWindow =
    
        open System
        open Gtk;

        type MyWindow() as this =
            inherit Window("MainWindow")
            
            do this.SetDefaultSize(400,300)
            do this.Add(new Button("click"))
            do this.Add(new Label("foo"))
            do this.DeleteEvent.AddHandler(fun o e -> this.OnDeleteEvent(o,e))
            do this.ShowAll()
            
            member this.OnDeleteEvent(o,e:DeleteEventArgs) = 
                Application.Quit ()
                e.RetVal <- true
