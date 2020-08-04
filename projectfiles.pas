unit ProjectFiles;

{$mode objfpc}{$H+}

interface

uses
  ShellCtrls, ComCtrls, StdCtrls, LazLoggerBase, LCLType, ProjectIntf, Menus, Controls,
  Classes, SysUtils, MenuIntf, StrUtils, LazIDEIntf, IDECommands, FileUtil,
  IDEWindowIntf, LazFileUtils, IDEMsgIntf, IDEExternToolIntf, SrcEditorIntf,
  Forms, CodeToolManager, CodeCache, Dialogs, GraphType, Graphics, ExtCtrls;

type

  { TABProjectFiles }

  TABProjectFiles = class(TForm)

    ABAllFilesShellTreeView: TShellTreeView;
    ABPageControl: TPageControl;
    ABProjectFilesTreeView: TTreeView;
    AllFilesPopupMenu: TPopupMenu;
    AllFilesAddItemToProjectMenuItem: TMenuItem;
    AllFilesOpenItem: TMenuItem;
    ProjectFilesRemoveMenuItem: TMenuItem;
    ProjectFilesCreateNewItemMenuItem: TMenuItem;
    ProjectFilesOpenMenuItem: TMenuItem;
    ProjectFilesRenameMenuItem: TMenuItem;
    ProjectFilesDeleteMenuItem: TMenuItem;
    ProjectFilesRefreshMenuItem: TMenuItem;
    ProjectFilesPopupMenu: TPopupMenu;
    ProjectSheet: TTabSheet;
    AllFilesSheet: TTabSheet;
    AllFilesProjectDirectoryRadioButton: TRadioButton;
    AllFilesRootDirectoryRadioButton: TRadioButton;
    AllFilesRadioGroup: TRadioGroup;


    RootNode: TTreeNode;
    UnitNode: TTreeNode;
    NonUnitsNode: TTreeNode;
    OthersNode: TTreeNode;

  const UnitNodeName : string = 'Units';
    NonUnitNodeName: string = 'Non Units';
    OthersNodeName: string = 'Others (Non Project Files)';


  procedure ABAllFilesShellTreeViewDblClick(Sender: TObject);
    procedure ABProjectFilesTreeViewDblClick(Sender: TObject);
    procedure AllFilesAddItemToProjectMenuItemClick(Sender: TObject);
    procedure AllFilesOpenItemClick(Sender: TObject);
    procedure AllFilesPopupMenuPopup(Sender: TObject);
    procedure AllFilesRadioGroupClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure ProjectFilesRemoveMenuItemClick(Sender: TObject);
    procedure ProjectFilesCreateNewItemMenuItemClick(Sender: TObject);
    procedure ProjectFilesDeleteMenuItemClick(Sender: TObject);
    procedure ProjectFilesOpenSelectedItem;
    procedure ProjectFilesOpenMenuItemClick(Sender: TObject);
    procedure ProjectFilesPopupMenuPopup(Sender: TObject);
    procedure ProjectFilesRefreshMenuItemClick(Sender: TObject);
    procedure ProjectFilesRenameMenuItemClick(Sender: TObject);
    procedure ProjectFilesRefresh;
    procedure RefreshAllFilesProjectView;
    procedure RemoveProjectFile(const projFile: TLazProjectFile);
    function OnSaveEditorFile(Sender: TObject; aFile: TLazProjectFile;
    SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;

  public

    destructor Destroy();override;

  private
    procedure ABAllFilesShellTreeViewOpenSelectedItem;
    procedure AddNodeToTreeView(const PFile: TLazProjectFile);
    procedure BuildNode(const PFile: TLazProjectFile; const PNode: TTreeNode);
    procedure CreateNewProjectItem(Sender: TObject);
    procedure InitProjectTreeViewTopNodes;
    procedure OnSourceEditorWindowRemoved(Sender: TObject);
    procedure OnSourceEditorWindowAdded(Sender: TObject);

    function ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;


    procedure RemoveFileFromProjectAndTreeView(const projFile: TLazProjectFile);
    procedure RemoveProjectFileFromTreeView(const projFile: TLazProjectFile);


  end;

var
  ABProjectFiles: TABProjectFiles;
  ABProjectFilesMainWindowCreator: TIDEWindowCreator; // set by Register procedure

procedure ShowABProjectFilesMainWindow(Sender: TObject);
procedure Register; // Check the "Register Unit" of this unit in the package editor.implementation

implementation

{$R *.lfm}


destructor TABProjectFiles.Destroy();
begin
  LazarusIDE.RemoveHandlerOnProjectOpened(@Self.ProjectOpened);
  LazarusIDE.RemoveHandlerOnSaveEditorFile(@OnSaveEditorFile);
  SrcEditorIntf.SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorCreate, @Self.OnSourceEditorWindowAdded);
  SrcEditorIntf.SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorDestroy, @Self.OnSourceEditorWindowRemoved);
end;

procedure TABProjectFiles.RemoveFileFromProjectAndTreeView(
  const projFile: TLazProjectFile);
var
  SourceEditor: TSourceEditorInterface;
begin
  SourceEditor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(
    projFile.GetFullFilename);

  if SourceEditor <> nil then
  LazarusIDE.DoCloseEditorFile(
    SourceEditor, [cfQuiet]);

  RemoveProjectFile(projFile);
  RemoveProjectFileFromTreeView(projFile)
end;

function TABProjectFiles.ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Self.ProjectFilesRefresh;
  Self.RefreshAllFilesProjectView;
  exit(mrNo);
end;

procedure TABProjectFiles.ProjectFilesRenameMenuItemClick(Sender: TObject);
var filepath, filename, newfilepath, newfilename: string;
  node: TLazProjectFile;
begin

  node := TLazProjectFile(Self.ABProjectFilesTreeView.Selected.Data);

  IDECommands.ExecuteIDECommand(node, ecSaveAs);
  Self.ProjectFilesRefresh;

end;

procedure TABProjectFiles.ProjectFilesDeleteMenuItemClick(Sender: TObject);
var filename: string;
  count: integer;
  HasEncounteredError: boolean;
  PFile: TLazProjectFile;
begin

  count := 0;
  HasEncounteredError := False;
  if MessageDlg('Delete Selected Items', 'Do you want to delete selected items', TMsgDlgType.mtConfirmation, [mbYes, mbNo, mbCancel], 'Delete Items') <> mrYes then exit;
  while count < Self.ABProjectFilesTreeView.SelectionCount do
  begin
    if (not Assigned(Self.ABProjectFilesTreeView.Selections[count])) and
      (not Assigned(Self.ABProjectFilesTreeView.Selections[count].Data))then begin
     HasEncounteredError := True;
     continue;
    end;

    PFile := TLazProjectFile(Self.ABProjectFilesTreeView.Selections[count].Data);

    filename := CleanAndExpandFilename(PFile.GetFullFilename);

    if DeleteFile(filename) = false then HasEncounteredError := true
    else begin
      Self.RemoveFileFromProjectAndTreeView(PFile);
    end;

    inc(count);
  end;


  if HasEncounteredError then ShowMessage('Some files/directories where not deleted succesfully');
end;

procedure TABProjectFiles.CreateNewProjectItem(Sender: TObject);
begin
  IDECommands.ExecuteIDECommand(LazarusIDE.ActiveProject, IDECommands.ecNew);
end;

procedure TABProjectFiles.InitProjectTreeViewTopNodes;
var
  PNodeName: string;
begin
  if ABProjectFilesTreeView.Items.Count > 0
    then ABProjectFilesTreeView.Items.Clear;
    PNodeName := 'Current Project';

    if Assigned(LazarusIDE.ActiveProject) then PNodeName := PNodeName + ' (' + LazarusIDE.ActiveProject.GetTitleOrName + ')';

    RootNode := ABProjectFilesTreeView.Items.AddFirst(nil, PNodeName);
    UnitNode := ABProjectFilesTreeView.Items.AddChild(RootNode, UnitNodeName);
    NonUnitsNode
      := ABProjectFilesTreeView.Items.AddChild(RootNode, NonUnitNodeName);
    OthersNode
      := ABProjectFilesTreeView.Items.AddChild(RootNode, OthersNodeName);
end;

procedure TABProjectFiles.OnSourceEditorWindowRemoved(Sender: TObject);
var
  projFile: TLazProjectFile;
begin
  if Sender.InheritsFrom(TSourceEditorInterface) then
   begin
    projFile := TSourceEditorInterface(Sender).GetProjectFile;
    if System.Length(ExtractFilePath(projFile.GetFullFilename)) = 0 then begin
     Self.RemoveProjectFileFromTreeView(projFile);
    end;
   end;

end;

procedure TABProjectFiles.OnSourceEditorWindowAdded(Sender: TObject);
var PFile: TLazProjectFile;
  SourceFile: TSourceEditorInterface;
  count: Integer;
begin
  if Sender.InheritsFrom(TSourceEditorInterface) then
   begin
    SourceFile := TSourceEditorInterface(Sender);
    for count := 0 to LazarusIDE.ActiveProject.FileCount - 1 do begin
      PFile := LazarusIDE.ActiveProject.Files[count];

      if PFile = nil then continue;
      if SameFilename(SourceFile.FileName, PFile.Filename) then begin
       Self.AddNodeToTreeView(PFile);
       exit;
      end;
    end;

   end;
end;

procedure TABProjectFiles.FormCreate(Sender: TObject);
begin

  if Assigned(LazarusIDE.ActiveProject) then begin
   Self.ProjectFilesRefresh;
   Self.RefreshAllFilesProjectView;
   end
   else Self.InitProjectTreeViewTopNodes;

  LazarusIDE.AddHandlerOnProjectOpened(@ProjectOpened, True);
  LazarusIDE.AddHandlerOnSaveEditorFile(@OnSaveEditorFile, True);
  SrcEditorIntf.SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate, @OnSourceEditorWindowAdded);
  SrcEditorIntf.SourceEditorManagerIntf.RegisterChangeEvent(semEditorDestroy, @OnSourceEditorWindowRemoved);
end;

procedure TABProjectFiles.ProjectFilesRemoveMenuItemClick(Sender: TObject);
var
  projFile: TLazProjectFile;
begin

  if Self.ABProjectFilesTreeView.Selected.Data <> nil then begin
    projFile := TLazProjectFile(ABProjectFilesTreeView.Selected.Data);
    RemoveFileFromProjectAndTreeView(projFile);
  end;
end;

procedure TABProjectFiles.ProjectFilesCreateNewItemMenuItemClick(Sender: TObject
  );
begin
  Self.CreateNewProjectItem(Sender);
end;

procedure TABProjectFiles.ProjectFilesOpenSelectedItem;
begin
  if (ABProjectFilesTreeView.Selected <> nil) and (ABProjectFilesTreeView.Selected.Data <> nil) then
      LazarusIDE.DoOpenEditorFile(TLazProjectFile(
        ABProjectFilesTreeView.Selected.Data).GetFullFilename, 0, 0, [
        ofQuiet, ofDoLoadResource, ofVirtualFile]);
end;

procedure TABProjectFiles.ProjectFilesOpenMenuItemClick(Sender: TObject);
begin
  Self.ProjectFilesOpenSelectedItem;
end;

procedure TABProjectFiles.ProjectFilesPopupMenuPopup(Sender: TObject);
var
  isVirtual: boolean;
  node: TLazProjectFile;
begin

  if (Self.ABProjectFilesTreeView.SelectionCount <> 1)
    or ((Self.ABProjectFilesTreeView.SelectionCount = 1)
    and (Self.ABProjectFilesTreeView.Selected.Data = nil)) then begin
      ProjectFilesOpenMenuItem.Enabled := False;
      ProjectFilesRenameMenuItem.Enabled := False;
      ProjectFilesRemoveMenuItem.Enabled := False;
  end else begin
    ProjectFilesOpenMenuItem.Enabled := True;

    if (Self.ABProjectFilesTreeView.Selected <> nil) and (Self.ABProjectFilesTreeView.Selected.Data <> nil) then begin
      node := TLazProjectFile(Self.ABProjectFilesTreeView.Selected.Data);
      try
        isVirtual := System.Length(ExtractFilePath(node.GetFullFilename)) = 0;
      except
        isVirtual := True
      end;
    end;

    if not isVirtual then
    ProjectFilesRenameMenuItem.Enabled := True
    else ProjectFilesRenameMenuItem.Enabled := False;

    if (node <> nil) and node.IsPartOfProject then begin
      ProjectFilesRemoveMenuItem.Enabled := True;
    end else ProjectFilesRemoveMenuItem.Enabled := False;

  end;

  if (Self.ABProjectFilesTreeView.SelectionCount > 1)
    or ((Self.ABProjectFilesTreeView.SelectionCount = 1) and not isVirtual)
    then begin
     ProjectFilesDeleteMenuItem.Enabled := True;
    end
  else begin
    ProjectFilesDeleteMenuItem.Enabled := False;
  end;
end;

procedure TABProjectFiles.ProjectFilesRefreshMenuItemClick(Sender: TObject);
begin
  Self.ProjectFilesRefresh;
end;

procedure TABProjectFiles.ProjectFilesRefresh;
var count: integer;
  PFile: TLazProjectFile;

begin
  InitProjectTreeViewTopNodes;

  for count := 0 to LazarusIDE.ActiveProject.FileCount - 1 do
    begin
      PFile := LazarusIDE.ActiveProject.Files[count];
      if PFile = nil then continue;
      AddNodeToTreeView(PFile);
    end;

  RootNode.Expand(False);
  UnitNode.Expand(True);
end;

procedure TABProjectFiles.RefreshAllFilesProjectView;
begin
  if AllFilesProjectDirectoryRadioButton.Checked then
   begin
    Self.ABAllFilesShellTreeView.Root := LazarusIDE.ActiveProject.Directory;
   end else if AllFilesRootDirectoryRadioButton.Checked then begin
    Self.ABAllFilesShellTreeView.Root := '';
   end;
end;

procedure TABProjectFiles.RemoveProjectFileFromTreeView(
  const projFile: TLazProjectFile);
var
  node: TTreeNode;
begin
  node := Self.ABProjectFilesTreeView.Items.FindNodeWithData(projFile);
  if node = nil then exit;

  {Let us remove empty trees}
  while (node.GetNextSibling = nil) and (node.GetPrevSibling = nil) do begin
    if (node.Parent = Self.UnitNode) or (node.Parent = Self.NonUnitsNode) or (node.Parent = Self.OthersNode) then break;
    node := node.Parent;
  end;

  node.Delete;
end;

procedure TABProjectFiles.RemoveProjectFile(const projFile: TLazProjectFile);
var
  c: integer;
begin
  if (projFile = nil) or (not projFile.IsPartOfProject) then exit;
  for c := 0 to (LazarusIDE.ActiveProject.FileCount - 1) do begin
    if projFile = LazarusIDE.ActiveProject.Files[c] then begin
      LazarusIDE.ActiveProject.RemoveUnit(c, True);
    end;
  end;
end;

function TABProjectFiles.OnSaveEditorFile(Sender: TObject; aFile: TLazProjectFile; SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
begin

  if SaveStep <> sefsBeforeWrite then exit;

  if not FileExists(TargetFilename) then Self.ProjectFilesRefresh;

  exit(mrOk);
end;

procedure TABProjectFiles.AddNodeToTreeView(const PFile: TLazProjectFile);
begin

  if not PFile.IsPartOfProject then begin
    BuildNode(PFile, OthersNode);
  end
  else if FilenameIsPascalUnit(PFile.Filename) then begin
    BuildNode(PFile, UnitNode);
  end else
  begin
    BuildNode(PFile, NonUnitsNode)
  end;
end;

procedure TABProjectFiles.BuildNode(const PFile: TLazProjectFile;
  const PNode: TTreeNode);
var filename, f: string;
  dirs: TStringArray;
  tempnode, cntempnode: TTreeNode;
begin
  filename := PFile.GetShortFilename(True);
  f := TrimFilename(filename);

  f := CreateRelativePath(f, LazarusIDE.ActiveProject.Directory);

  tempnode := PNode;

  if System.Length(f) > 0 then begin
    dirs := f.Split(System.DirectorySeparator);

    for f in dirs do begin

      cntempnode := tempnode.FindNode(f);

      if Assigned(cntempnode) then begin
        tempnode := cntempnode;
      end else begin
        tempnode := ABProjectFilesTreeView.Items.AddChild(tempnode, f);
      end;
    end;

    tempnode.Data := PFile;
  end;
end;

procedure TABProjectFiles.ABProjectFilesTreeViewDblClick(Sender: TObject);
begin
  ProjectFilesOpenSelectedItem;
end;

procedure TABProjectFiles.AllFilesAddItemToProjectMenuItemClick(Sender: TObject);
var
  SourceEditor: TSourceEditorInterface;
begin
  if (ABAllFilesShellTreeView.Selected <> nil) then begin

   LazarusIDE.DoOpenEditorFile(ABAllFilesShellTreeView.Selected.GetTextPath, 0, 0, [ofQuiet, ofAddToProject, ofDoLoadResource]);
  end;
end;

procedure TABProjectFiles.AllFilesOpenItemClick(Sender: TObject);
begin
  if ((FileGetAttr(ABAllFilesShellTreeView.Selected.GetTextPath) and faDirectory) <> 0) then
  Self.ABAllFilesShellTreeView.Selected.Expand(False)
  else
  Self.ABAllFilesShellTreeViewOpenSelectedItem;
end;

procedure TABProjectFiles.AllFilesPopupMenuPopup(Sender: TObject);
begin
  if ABAllFilesShellTreeView.Selected <> nil then begin
   if (not FilenameIsPascalUnit(CleanAndExpandFilename(ABAllFilesShellTreeView.Selected.GetTextPath))) then
    Self.AllFilesAddItemToProjectMenuItem.Enabled := False else Self.AllFilesAddItemToProjectMenuItem.Enabled := True;
  end;

end;

procedure TABProjectFiles.AllFilesRadioGroupClick(Sender: TObject);
begin
  RefreshAllFilesProjectView;
end;


procedure TABProjectFiles.ABAllFilesShellTreeViewDblClick(Sender: TObject);
begin
  Self.ABAllFilesShellTreeViewOpenSelectedItem;
end;

procedure TABProjectFiles.ABAllFilesShellTreeViewOpenSelectedItem;
begin
   if (ABAllFilesShellTreeView.Selected <> nil) then
      LazarusIDE.DoOpenEditorFile(ABAllFilesShellTreeView.Selected.GetTextPath, 0, 0, [
        ofQuiet, ofDoLoadResource, ofVirtualFile]);
end;


procedure ShowABProjectFilesMainWindow(Sender: TObject);
begin
  if (ABProjectFilesMainWindowCreator = nil) then exit;

  IDEWindowCreators.ShowForm(ABProjectFilesMainWindowCreator.FormName, true);
end;

procedure CreateABProjectFilesMainWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName, 'ABProjectFilesMainWindow') <> 0 then begin
    DebugLn(['ERROR: CreateABProjectFilesMainWindow: there is already a form with this '
      + 'name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm, TABProjectFiles, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name := aFormName;
  ABProjectFiles := AForm as TABProjectFiles;
end;

procedure Register;
var
  CmdCatViewMenu: TIDECommandCategory;
  ViewABProjectFilesMainWindowCommand: TIDECommand;
  MenuItemCaption: String;
begin
  // register shortcut and menu item
  MenuItemCaption := 'Project Files'; // <- this caption should be replaced by a resourcestring
  // search shortcut category
  CmdCatViewMenu := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  ViewABProjectFilesMainWindowCommand := RegisterIDECommand(CmdCatViewMenu,
    'ViewABProjectFilesMainWindow',
    MenuItemCaption,
    IDEShortCut(VK_P, [ssCtrl, ssShift]), // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowABProjectFilesMainWindow);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows,
    'ViewABProjectFilesMainWindow',
    MenuItemCaption, nil, nil, ViewABProjectFilesMainWindowCommand);

  // register dockable Window
  ABProjectFilesMainWindowCreator := IDEWindowCreators.Add(
    'ABProjectFilesMainWindow',
    @CreateABProjectFilesMainWindow, nil,
    '100', '100', '400', '600'  // default place at left=100, top=100, right=300, bottom=300
      // you can also define percentage values of screen or relative positions, see wiki
    );
end;

end.

