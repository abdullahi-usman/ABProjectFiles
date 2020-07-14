{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ABProjectFiles;

{$warn 5023 off : no warning about unused units}
interface

uses
  ProjectFiles, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ProjectFiles', @ProjectFiles.Register);
end;

initialization
  RegisterPackage('ABProjectFiles', @Register);
end.
