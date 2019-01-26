module ERModel =
    open System
    open System.Collections.Generic
    open System.Linq
    open System.Text
    open System.Threading.Tasks
    open Oracle.ManagedDataAccess.Client
    open System.Windows
    open System.Windows.Controls
    open System.Windows.Markup
    open FsXaml
    open System.Windows

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

    type MainWindow = XAML<"MainWindow.xaml">

    [<STAThread>]
    [<EntryPoint>]
    let main argv = 
        let conn = new OracleConnection()
        conn.ConnectionString <-
            "User Id=ENTWICKLER;Password=1234;Data Source=127.0.0.1:1521/ORA12C"
        conn.Open()

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
         
        let app = Application()
        let w = MainWindow()
        let rec addColumns (tableItem : TreeViewItem) (table : Table) (dict : Dictionary<Table, List<Column>>) = 
            for currentColumn in dict.[table] do
                let columnItem =
                    new TreeViewItem(
                        Header =
                            sprintf
                                "%s (%snull) => %s%s.%s"
                                currentColumn.columnName
                                (if currentColumn.columnNullable then "" else "not ")
                                (if currentColumn.referencedOwner = table.tableOwner then "" else currentColumn.referencedOwner + ".")
                                currentColumn.referencedTableName
                                currentColumn.referencedColumnName)
                tableItem.Items.Add(columnItem) |> ignore
                if dict.[{ tableOwner = table.tableOwner; tableName = currentColumn.referencedTableName}].Any() then
                    let placeholderItem = TreeViewItem(Header = "Loading...")
                    columnItem.Expanded.Add(
                        fun _ ->
                            if columnItem.Items.Count = 1
                               && string (columnItem.Items.[0] :?> TreeViewItem).Header = "Loading..."
                            then
                                columnItem.Items.Clear()
                                let referencedTable =
                                    { tableOwner = currentColumn.referencedOwner
                                      tableName = currentColumn.referencedTableName
                                    }
                                addColumns
                                    columnItem
                                    referencedTable
                                    dict


                    )
                    columnItem.Items.Add placeholderItem |> ignore
        for tableData in dict do
            let currentTable = tableData.Key
            let currentColumns = tableData.Value
            let tableItem = TreeViewItem(Header = currentTable.tableName)
            addColumns tableItem tableData.Key dict
            w.treeView.Items.Add(tableItem) |> ignore
        app.Run(w)

