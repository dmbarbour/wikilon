namespace wikilon
    module Main =
        
        open System
        open Gtk
    
        [<EntryPoint>]
        let Main(args) = 
            Application.Init()
            let win = new MainWindow.MyWindow()
            win.Show()
            Application.Run()
            0

