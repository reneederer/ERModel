namespace ERModel
open System
open System.Collections.Generic
open System.Linq
open Oracle.ManagedDataAccess.Client
open System.Windows.Controls
open FsXaml
open System.Windows
open FSharp.Data
open System.IO
open System.Windows.Threading
open System.Windows.Interop
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
module NativeMethods =
    let HWND_BROADCAST = 0xffff
    [<DllImport("user32")>]
    extern bool public PostMessage(IntPtr hwnd, int msg, IntPtr wparam, IntPtr lparam)
    [<DllImport("user32")>]
    extern int RegisterWindowMessage(string message)
    let WM_SHOWME = RegisterWindowMessage("WM_SHOWME")

module View =
    open System.Windows.Media.Imaging
    open System.Windows.Data
    open System.Collections.ObjectModel

    type Column =
        { columnName : string
          columnNullable : bool
          columnType : string
          referencedOwner : string
          referencedTableName : string
          referencedColumnName : string
        }
    type Table =
        { tableOwner : string
          tableName : string
        }

    [<CLIMutable>]
    type ColumnView =
        { Name : string
          Nullable : string
          Type : string
        }

    type MainWindowBase = XAML<"MainWindow.xaml">
    type MainWindow() =
        inherit MainWindowBase()

        override this.OnSourceInitialized(e) =
            base.OnSourceInitialized(e)
            let source = PresentationSource.FromVisual(this) :?> HwndSource;
            source.AddHook(
                fun hwnd msg wParam lParam handled ->
                    if msg = NativeMethods.WM_SHOWME then
                        this.Show()
                        MessageBox.Show("Already open!") |> ignore
                        this.Activate() |> ignore
                        this.Topmost <- true
                        this.Topmost <- false
                    IntPtr.Zero)

    let w = new MainWindow()
    [<CLIMutable>]
    type C = ObservableCollection<ColumnView>
    let columnDetails = new C()
    columnDetails.Add(
        { Name = ""
          Type = ""
          Nullable = ""
        })
    type App() =
        inherit Application()
        static member singleton = new Mutex(true, "ERModel");

        [<STAThread>]
        member this.Application_Startup(sender, e) =
            if App.singleton.WaitOne(TimeSpan.Zero, true) then
                App.singleton.ReleaseMutex()
                w.WindowState <- WindowState.Maximized
                w.Show();
            else
                NativeMethods.PostMessage(
                    IntPtr(NativeMethods.HWND_BROADCAST),
                    NativeMethods.WM_SHOWME,
                    IntPtr.Zero,
                    IntPtr.Zero) |> ignore
                this.Shutdown()

    type Config = XmlProvider<"Config.xml">
    let mutable config = Config.Load(Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Config.xml"))

    let getColumnDetails tableOwner tableName (conn : OracleConnection) =
        use command = conn.CreateCommand(CommandText = sprintf "
            select column_name, nullable, data_type, data_length, data_precision, data_scale
            from all_tab_columns
            where owner = '%s' and table_name = '%s'
            order by column_name" tableOwner tableName)
        use reader = command.ExecuteReader()
        [ while reader.Read() do
            yield
                { Name = reader.GetString(0)
                  Nullable = match reader.GetString(1) with | "N" -> "No" | "Y" -> "Yes" | _ -> "Error"
                  Type =
                      match reader.GetString(2) with
                      | "VARCHAR2" -> sprintf "VARCHAR2(%i)" (reader.GetInt32(3))
                      | v ->
                          match reader.IsDBNull(4), reader.IsDBNull(5) with
                          | true, true -> v
                          | false, true -> sprintf "%s(%i)" v (reader.GetInt32(4))
                          | false, false -> sprintf "%s(%i,%i)" v (reader.GetInt32(4)) (reader.GetInt32(5))
                          | true, false -> sprintf "%s(%i)" v (reader.GetInt32(5))
                }
        ] |> Seq.ofList


    let loadDict (conn : OracleConnection) =
        let owner = "ENTWICKLER"
        let dict = new Dictionary<Table, List<Column>>()
        use command = conn.CreateCommand(CommandText = sprintf "
        select allt.table_name, atc.column_name, atc.nullable, rucc.owner, rucc.table_name, ratc.column_name
  from all_tables allt
  left join user_constraints uc on allt.owner = uc.owner and allt.table_name = uc.table_name and uc.constraint_type = 'R'
  left join user_cons_columns ucc on (uc.owner = ucc.owner and uc.table_name = ucc.table_name and uc.constraint_name = ucc.constraint_name)
  left join all_tab_columns atc on (ucc.owner = atc.owner and ucc.table_name = atc.table_name and ucc.column_name = atc.column_name)
  left join user_cons_columns rucc on (uc.r_owner = rucc.owner and uc.r_constraint_name = rucc.constraint_name and ucc.position = rucc.position)
  left join all_tab_columns ratc on (rucc.owner = ratc.owner and rucc.table_name = ratc.table_name and rucc.column_name = ratc.column_name)
  where upper(allt.owner) = '%s' order by allt.table_name, atc.column_name" owner)
        use reader = command.ExecuteReader()
        while reader.Read() do
            let tableName = reader.GetString(0)
            let table = { tableOwner = owner; tableName = tableName }
            if not <| dict.ContainsKey(table) then
                dict.Add(table, new List<Column>())
            if not <| reader.IsDBNull(1) then
                let columnName = reader.GetString(1)
                let nullable = reader.GetString(2)
                let r_owner = reader.GetString(3)
                let r_tableName = reader.GetString(4)
                let r_columnName = reader.GetString(5)
                dict.[table].Add(
                    { columnName = columnName
                      columnNullable = nullable = "Y"
                      columnType = ""
                      referencedOwner = r_owner
                      referencedTableName = r_tableName
                      referencedColumnName = r_columnName
                    })
        dict

    let withConnection (f : OracleConnection -> 'b) connectionString =
        use conn = new OracleConnection(connectionString)
        conn.Open()
        f conn

    type HeaderType =
        | Normal
        | Reversed

    let app = App()

    let rec createTreeViewItem (table : Table) (oColumn : option<Column>) headerType (dict : Dictionary<Table, List<Column>>) =
        let linkTable =
            match headerType, oColumn with
            | Normal, Some column ->
                { tableOwner = column.referencedOwner
                  tableName = column.referencedTableName
                }
            | Normal, None
            | Reversed, _ ->
                { tableOwner = table.tableOwner
                  tableName = table.tableName
                }
        let t =
            new TreeViewItem(
                Header =
                    match headerType, oColumn with
                    | Normal, Some column ->
                        sprintf
                            "%s.%s (%snull) => %s.%s"
                            table.tableName
                            column.columnName
                            (if column.columnNullable then "" else "not ")
                            column.referencedTableName 
                            column.referencedColumnName
                    | Normal, None -> table.tableName
                    | Reversed, Some column ->
                        sprintf
                            "%s.%s <= %s.%s (%snull)"
                            column.referencedTableName 
                            column.referencedColumnName
                            table.tableName
                            column.columnName
                            (if column.columnNullable then "" else "not ")
                    | Reversed, None -> failwith "Unsupported condition: Reversed, None")
        if dict.[linkTable].Count >= -1 then
            let placeholderItem = TreeViewItem(Header = "Loading...")
            t.Expanded.Add(
                fun _ ->
                    if t.Items.Count = 1 && t.Items.[0] :?> TreeViewItem = placeholderItem
                    then replaceTreeViewColumns
                            t
                            linkTable
                            dict
            )
            t.Items.Add placeholderItem |> ignore
        t.Selected.Add(fun evt ->
            withConnection
                (fun conn ->
                    columnDetails.Clear()
                    for item in getColumnDetails linkTable.tableOwner linkTable.tableName conn do
                        columnDetails.Add(item)
                    w.tbTable.Text <- sprintf "%s.%s" linkTable.tableOwner linkTable.tableName
                    evt.Handled <- true
                )
                config.ConnectionStrings.[w.cmbConnection.SelectedIndex]
        )
        t
    and replaceTreeViewColumns
        (parent : ItemsControl)
        (table : Table)
        (dict : Dictionary<Table, List<Column>>) =
        parent.Items.Clear()
        for column in dict.[table] do
            let t =
                createTreeViewItem
                    table
                    (Some column)
                    Normal
                    dict
            parent.Items.Add t |> ignore
        let rDict =
            dict
                .Where(fun x ->
                    x.Value.Any(fun v -> v.referencedTableName = table.tableName))
                .Select(fun x -> x.Key, x.Value.Where(fun v -> v.referencedTableName = table.tableName))
        if rDict.Any() then
            let rTablesItem = TreeViewItem(Header = "_r")
            for kv in rDict do
                let rTable, rColumns = kv
                for rColumn in rColumns do
                    let t =
                        createTreeViewItem
                            rTable
                            (Some rColumn)
                            Reversed
                            dict
                    rTablesItem.Items.Add t |> ignore
            parent.Items.Add rTablesItem |> ignore


    let createTreeView (parent : ItemsControl) (dict : Dictionary<Table, List<Column>>) = 
        for kv in dict do
            let (currentTable, currentColumns) = (kv.Key, kv.Value)
            let t =
                createTreeViewItem
                    { tableOwner = currentTable.tableOwner; tableName = currentTable.tableName }
                    None
                    Normal
                    dict
            parent.Items.Add t |> ignore

    let load connectionString =
        try
            let dict = withConnection loadDict config.ConnectionStrings.[w.cmbConnection.SelectedIndex]
            createTreeView w.treeView dict
        with
        | err ->
            MessageBox.Show("Sorry, an error occurred:\n" + err.ToString()) |> ignore
    let initCmbConnectionString() =
        w.cmbConnection.ItemsSource <- config.ConnectionStrings
        if w.cmbConnection.Items.Count >= 1 then
            w.cmbConnection.SelectedIndex <- 0

    [<STAThread>]
    [<EntryPoint>]
    let main argv = 
        w.Icon <-
            new BitmapImage(
                Uri (Path.Combine( System.AppDomain.CurrentDomain.BaseDirectory, "images", "r-icon.ico")))
        let a = (Path.Combine( System.AppDomain.CurrentDomain.BaseDirectory, "images", "edit-icon.png"))
        w.btnEditConfig.Content <-
            Image(Source = new BitmapImage(Uri (Path.Combine( System.AppDomain.CurrentDomain.BaseDirectory, "images", "edit-icon.png"))))
        w.btnEditConfig.Click.Add(fun _ ->
            Process.Start(Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Config.xml")) |> ignore)
        use watcher =
            new FileSystemWatcher(
                Path = System.AppDomain.CurrentDomain.BaseDirectory,
                NotifyFilter = (NotifyFilters.LastWrite ||| NotifyFilters.FileName ||| NotifyFilters.DirectoryName),
                Filter = "Config.xml",
                EnableRaisingEvents = true)
        watcher.Changed.Add(fun e ->
            (w.Dispatcher.BeginInvoke(new Action(fun () -> w.treeView.Items.Clear())).Wait()) |> ignore
            (w.Dispatcher.BeginInvoke(new Action(fun () -> w.cmbConnection.ItemsSource <- [])).Wait()) |> ignore
            config <- Config.Load(Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "Config.xml"))
            (w.Dispatcher.BeginInvoke(new Action(initCmbConnectionString)).Wait()) |> ignore)
        w.tbSearch.KeyUp.Add(fun _ ->
            for item in w.treeView.Items do
                let treeViewItem = item :?> TreeViewItem
                if treeViewItem.Header.ToString().ToUpper().Contains(w.tbSearch.Text.ToUpper()) then
                    treeViewItem.Visibility <- Visibility.Visible
                else
                    treeViewItem.Visibility <- Visibility.Collapsed
        )
        w.dgColumns.ItemsSource <- columnDetails
        w.cmbConnection.SelectionChanged.Add(fun _ ->
            if w.cmbConnection.SelectedIndex >= 0 then
                load config.ConnectionStrings.[w.cmbConnection.SelectedIndex])
        w.cmbConnection.KeyUp.Add(fun evt ->
            if evt.Key = Input.Key.Enter then
                load w.cmbConnection.Text)
        initCmbConnectionString()
        app.Run w
