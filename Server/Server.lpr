program PVK;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, UtilsUnit;

{$R *.res}

begin
  Application.Title:='PVK';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TServerSideForm, ServerSideForm);
  Application.Run;
end.

