module ERModel =
    open System
    open System.Collections.Generic
    open System.Linq
    open Oracle.ManagedDataAccess.Client
    open System.Windows.Controls
    open FsXaml
    open System.Windows
    open FSharp.Data

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

    type ColumnDetails =
        { columnName : string
          columnType : string
          nullable : string
        }

    type ColumnView =
        { columnName : string
          columnType : string
          columnNullable : string
        }

    type MainWindow = XAML<"MainWindow.xaml">
    type Config = XmlProvider<"Config.xml">
    let config = Config.GetSample()

    let getColumnDetails tableOwner tableName (conn : OracleConnection) =
        use command = conn.CreateCommand(CommandText = sprintf "
            select column_name, data_type, nullable
            from all_tab_columns
            where owner = '%s' and table_name = '%s'
            order by column_name" tableOwner tableName)
        use reader = command.ExecuteReader()
        [ while reader.Read() do
            yield
                { columnName = reader.GetString(0)
                  columnType = reader.GetString(1)
                  columnNullable = reader.GetString(2)
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
    
    [<STAThread>]
    [<EntryPoint>]
    let main argv = 
        let app = Application()
        let w = MainWindow()
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
                        w.dgColumns.ItemsSource <-
                            getColumnDetails linkTable.tableOwner linkTable.tableName conn
                        evt.Handled <- true
                    )
                    config.ConnectionStrings.[w.cmbConnection.SelectedIndex].Value 
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

        for connectionString in config.ConnectionStrings do
            w.cmbConnection.Items.Add(connectionString.Name) |> ignore
        if w.cmbConnection.Items.Count >= 1 then
            w.cmbConnection.SelectedIndex <- 0
            let dict = withConnection loadDict config.ConnectionStrings.[w.cmbConnection.SelectedIndex].Value
            createTreeView w.treeView dict
        app.Run(w)

