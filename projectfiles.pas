unit ProjectFiles;

{$mode objfpc}{$H+}

interface

uses
  ShellCtrls, ComCtrls, StdCtrls, Menus, Controls, Classes, SysUtils, MenuIntf, StrUtils, LazIDEIntf, IDEWindowIntf, SrcEditorIntf, Forms;

type
  TABProjectFiles = class(TForm)

  private

  public
    FilesList: TShellTreeView;
    procedure FormActivate(Sender: TObject);
    procedure ProjectRefreshClicked(Sender: TObject);
    procedure OnFileNameClicked(Sender: TObject);
    procedure ProjectRefresh();
  end;

procedure Register;
implementation

var ABProjectFiles: TABProjectFiles;

procedure TABProjectFiles.ProjectRefresh();
begin

  if CompareText(Self.FilesList.Root, LazarusIDE.ActiveProject.Directory) <> 0 then
  begin
     Self.FilesList.ShowRoot:= true;
     Self.FilesList.Root := LazarusIDE.ActiveProject.Directory
  end;

end;

procedure TABProjectFiles.ProjectRefreshClicked(Sender: TObject);
begin

  Self.ProjectRefresh();

end;

procedure TABProjectFiles.FormActivate(Sender: TObject);
begin

  Self.ProjectRefresh();

end;

procedure TABProjectFiles.OnFileNameClicked(Sender: TObject);
begin
   LazarusIDE.DoOpenEditorFile(ABProjectFiles.FilesList.Selected.GetTextPath, 0, 0, [ofOnlyIfExists]);
end;

procedure CreateMyIDEWindow
   (Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);

var refreshMenu : TMenuItem;
begin
  IDEWindowCreators.CreateForm(ABProjectFiles,TForm,DoDisableAutosizing,Application);

  ABProjectFiles.Name:= 'ABProjectFiles';
  ABProjectFiles.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  ABProjectFiles.ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
  ABProjectFiles.ChildSizing.EnlargeVertical := crsHomogenousChildResize;
  ABProjectFiles.ChildSizing.ShrinkHorizontal:= crsHomogenousChildResize;
  ABProjectFiles.ChildSizing.ShrinkVertical:= crsHomogenousChildResize;



  ABProjectFiles.Menu := TMainMenu.Create(ABProjectFiles);
  ABProjectFiles.Menu.Parent := ABProjectFiles;
  ABProjectFiles.Menu.Name := 'Menu';

  refreshMenu := TMenuItem.Create(ABProjectFiles.Menu);
  refreshMenu.Caption := 'Refresh';
  refreshMenu.OnClick := @ABProjectFiles.ProjectRefreshClicked;

  ABProjectFiles.Menu.Items.Add(refreshMenu);

  ABProjectFiles.FilesList := TShellTreeView.Create(ABProjectFiles);
  ABProjectFiles.FilesList.Parent := ABProjectFiles;
  ABProjectFiles.FilesList.FileSortType:=fstFoldersFirst;
  ABProjectFiles.FilesList.ObjectTypes:=[otFolders,otNonFolders];
  ABProjectFiles.FilesList.Options:= [tvoAutoItemHeight,tvoHideSelection,tvoKeepCollapsedNodes,tvoShowButtons,tvoShowLines,tvoShowRoot,tvoToolTips,tvoThemedDraw];
  ABProjectFiles.FilesList.Root := LazarusIDE.ActiveProject.Directory;
  ABProjectFiles.FilesList.OnSelectionChanged := @ABProjectFiles.OnFileNameClicked;

  ABProjectFiles.OnActivate := @ABProjectFiles.FormActivate;
  AForm := ABProjectFiles;

end;



procedure StartMyTool(Ox: TObject);
begin
  IDEWindowCreators.ShowForm('ABProjectFiles',true);
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmViewMainWindows, 'ABProjectFiles', 'Project Files', nil, @StartMyTool);
  IDEWindowCreators.Add('ABProjectFiles',@CreateMyIDEWindow, nil,'100','50%','+300','+20%');
end;

end.

