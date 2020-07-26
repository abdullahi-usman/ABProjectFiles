unit ProjectFiles;

{$mode objfpc}{$H+}

interface

uses
  ShellCtrls, ComCtrls, StdCtrls, ProjectIntf, Menus, Controls, Classes, SysUtils,
  MenuIntf, StrUtils, LazIDEIntf, IDECommands, FileUtil, IDEWindowIntf, LazFileUtils, IDEMsgIntf,
  IDEExternToolIntf, SrcEditorIntf, Forms, CodeToolManager, CodeCache, Dialogs, GraphType,
  Graphics;

type

  { TABProjectFiles }

  TABProjectFiles = class(TForm)

  private

  public
    FilesList: TShellTreeView;
    procedure FormActivate(Sender: TObject);
    procedure ProjectRefreshClicked(Sender: TObject);
    procedure OnFileNameClicked(Sender: TObject);
    procedure ProjectRefresh();
    procedure OpenPopupMenu(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure EditSelectedFile(Sender: TObject);
    procedure DeleteSelectedFile(Sender: TObject);
    procedure CreateNewProjectItem(Sender: TObject);

    function ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;

    destructor Destroy();override;


  end;

procedure Register;
implementation

var ABProjectFiles: TABProjectFiles = nil;


destructor TABProjectFiles.Destroy();
begin
  LazarusIDE.RemoveHandlerOnProjectOpened(@Self.ProjectOpened);
end;

function TABProjectFiles.ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Self.ProjectRefresh();
  ProjectOpened := mrNone;
end;

procedure TABProjectFiles.ProjectRefresh();
var tt: string;
begin


  if (Self.FilesList <> nil) and (CompareText(Self.FilesList.Root, LazarusIDE.ActiveProject.Directory) <> 0) then
  begin
     Self.FilesList.Root := LazarusIDE.ActiveProject.Directory;
  end;

  Self.FilesList.Refresh(Self.FilesList.TopItem);

end;

procedure TABProjectFiles.OpenPopupMenu(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OpenMenuItem: TMenuItem;
  EditMenuItem: TMenuItem;
  DeleteMenuItem: TMenuItem;
  CreateNewMenuItem: TMenuItem;
  RefreshMenuItem: TMenuItem;
begin
  if ((Shift <> []) and (Button <> mbRight)) or (Self.FilesList.Selected = Self.FilesList.TopItem) then exit;

  Self.FilesList.PopupMenu := TPopupMenu.Create(Self);
  Self.FilesList.PopupMenu.Parent := Self;

  if (Self.FilesList.SelectionCount > 0) then
  begin
     if (Self.FilesList.SelectionCount = 1) then
     begin
        OpenMenuItem := TMenuItem.Create(Self.FilesList.PopupMenu);
        OpenMenuItem.Caption := 'Open Item';
        OpenMenuItem.Name := 'OpenItem';
        OpenMenuItem.OnClick := @Self.OnFileNameClicked;

        EditMenuItem := TMenuItem.Create(Self.FilesList.PopupMenu);
        EditMenuItem.Caption:= 'Edit Item Name';
        EditMenuItem.Name:= 'EditItem';
        EditMenuItem.OnClick := @Self.EditSelectedFile;

        Self.FilesList.PopupMenu.Items.Add(OpenMenuItem);
        Self.FilesList.PopupMenu.Items.Add(EditMenuItem);
     end;


    DeleteMenuItem := TMenuItem.Create(Self.FilesList.PopupMenu);
    DeleteMenuItem.Caption:='Delete';
    DeleteMenuItem.Name := 'DeleteItems';
    DeleteMenuItem.OnClick := @Self.DeleteSelectedFile;

    Self.FilesList.PopupMenu.Items.Add(DeleteMenuItem);
  end;

  CreateNewMenuItem := TMenuItem.Create(Self.FilesList.PopupMenu);
  CreateNewMenuItem.Caption := 'Create New Item';
  CreateNewMenuItem.Name := 'CreateNewItem';
  CreateNewMenuItem.OnClick := @Self.CreateNewProjectItem;

  RefreshMenuItem := TMenuItem.Create(Self.FilesList.PopupMenu);
  RefreshMenuItem.Caption := 'Refresh';
  RefreshMenuItem.Name := 'Refresh';
  RefreshMenuItem.OnClick := @Self.ProjectRefreshClicked;

  Self.FilesList.PopupMenu.Items.Add(CreateNewMenuItem);
  Self.FilesList.PopupMenu.Items.Add(RefreshMenuItem);

  Self.FilesList.PopupMenu.PopUp;
end;

procedure TABProjectFiles.EditSelectedFile(Sender: TObject);
var filepath, filename, newfilepath, newfilename: string;
begin

  filepath := CleanAndExpandFilename(Self.FilesList.Selected.GetTextPath);
  filename :=ExtractFilename(filepath);
  newfilename := InputBox('Edit Filename: ' + filename, 'Please Write a new filename', filename);
  if newfilename.IsEmpty or SameFileName(filename, newfilename) then exit;

  newfilepath := CreateAbsolutePath(newfilename, ExtractFilePath(filepath));

  if ((FileGetAttr(filename) and faDirectory) <> 0) then
  begin
    RenameFile(filepath, newfilepath);
  end
  else
  begin
    LazarusIDE.DoCloseEditorFile(filepath, [cfQuiet, cfSaveFirst]);
    RenameFile(filepath, newfilepath);

    if MessageDlg ('Open File', 'Due to the renaming operation, file may have been closed, do you want to open?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      LazarusIDE.DoOpenEditorFile(newfilepath, 0, 0, [ofQuiet, ofOnlyIfExists, ofAddToProject]);
  end;

  Self.ProjectRefresh();
end;

procedure TABProjectFiles.DeleteSelectedFile(Sender: TObject);
var filename, f: string;
  count: integer;
  HasEncounteredError: boolean;
begin

  count := 0;
  HasEncounteredError := False;
  if MessageDlg('Delete Selected Items', 'Do you want to delete selected items', TMsgDlgType.mtConfirmation, [mbYes, mbNo, mbCancel], 'Delete Items') <> mrYes then exit;
  while count < Self.FilesList.SelectionCount do
  begin
    filename := CleanAndExpandFilename(Self.FilesList.Selections[count].GetTextPath);

    if ((FileGetAttr(filename) and faDirectory) <> 0) then
    begin
      for f in FindAllFiles(filename) do
        begin
          LazarusIDE.DoCloseEditorFile(f, [cfQuiet]);
        end;

      if DeleteDirectory(filename, false) = false then HasEncounteredError := true;
    end
    else
    begin
      if DeleteFile(filename) = false then HasEncounteredError := true
      else LazarusIDE.DoCloseEditorFile(filename, [cfQuiet]);
    end;

    inc(count);
  end;

  if (HasEncounteredError = true) then ShowMessage('Some files/directories where not deleted succesfully');
  Self.ProjectRefresh();
end;

procedure TABProjectFiles.CreateNewProjectItem(Sender: TObject);
begin
  IDECommands.ExecuteIDECommand(LazarusIDE.ActiveProject, IDECommands.ecNew);
  Self.ProjectRefresh();
end;

procedure TABProjectFiles.ProjectRefreshClicked(Sender: TObject);
begin
  Self.ProjectRefresh();
end;

procedure TABProjectFiles.FormActivate(Sender: TObject);
begin
  if Sender.ClassNameIs('TABProjectFiles') then
     Self.ProjectRefresh();
end;

procedure TABProjectFiles.OnFileNameClicked(Sender: TObject);
var filename: string;
begin
  filename := Self.FilesList.Selected.GetTextPath;

  if (FileGetAttr(filename) and faDirectory) <> 0 then
  begin
    Self.FilesList.Selected.Expand(false);
    exit;
  end;

  LazarusIDE.DoOpenEditorFile(filename, 0, 0, [ofOnlyIfExists]);
end;

procedure CreateMyIDEWindow
   (Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);

var refreshMenu : TMenuItem;
begin
  if CompareText(aFormName, 'ABProjectFiles') <> 0 then exit;

  IDEWindowCreators.CreateForm(ABProjectFiles,TForm,DoDisableAutosizing,Application);

  ABProjectFiles.Name:= 'ABProjectFiles';
  with ABProjectFiles.ChildSizing do
  begin
    Layout := cclLeftToRightThenTopToBottom;
    EnlargeHorizontal := crsHomogenousChildResize;
    EnlargeVertical := crsHomogenousChildResize;
    ShrinkHorizontal:= crsHomogenousChildResize;
    ShrinkVertical:= crsHomogenousChildResize;
  end;

  ABProjectFiles.Menu := TMainMenu.Create(ABProjectFiles);
  ABProjectFiles.Menu.Parent := ABProjectFiles;
  ABProjectFiles.Menu.Name := 'Menu';

  refreshMenu := TMenuItem.Create(ABProjectFiles.Menu);
  refreshMenu.Caption := 'Refresh';
  refreshMenu.OnClick := @ABProjectFiles.ProjectRefreshClicked;

  ABProjectFiles.Menu.Items.Add(refreshMenu);

  ABProjectFiles.FilesList := TShellTreeView.Create(ABProjectFiles);
  with ABProjectFiles.FilesList do
  begin
    Parent := ABProjectFiles;
    SelectionColor := clHighlight;

    ExpandSignType := tvestPlusMinus;
    FileSortType:=fstFoldersFirst;
    ObjectTypes:=[otFolders,otNonFolders];
    Options := [tvoHotTrack, tvoAllowMultiSelect,tvoAutoItemHeight,tvoHideSelection,tvoKeepCollapsedNodes,tvoRowSelect,tvoShowButtons,tvoShowLines,tvoShowRoot,tvoToolTips,tvoThemedDraw];
    OnDblClick := @ABProjectFiles.OnFileNameClicked;
    MultiSelectStyle := [msControlSelect];
    OnMouseDown := @ABProjectFiles.OpenPopupMenu;
    ReadOnly := True;
  end;

  ABProjectFiles.OnActivate := @ABProjectFiles.FormActivate;

  LazarusIDE.AddHandlerOnProjectOpened(@ABProjectFiles.ProjectOpened, true);

  AForm := ABProjectFiles;

end;


procedure StartMyTool(Ox: TObject);
begin

  if not IDEWindowCreators.GetForm('ABProjectFiles', true).Visible then
  begin
    IDEWindowCreators.ShowForm('ABProjectFiles',true);
  end;

end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmViewMainWindows, 'ABProjectFiles', 'Project Files', nil, @StartMyTool);

  if IDEWindowCreators.GetForm('ABProjectFiles', false) = nil then
  begin
    IDEWindowCreators.Add('ABProjectFiles',@CreateMyIDEWindow, nil,'100','50%','+300','+30%');
  end;
end;

end.

