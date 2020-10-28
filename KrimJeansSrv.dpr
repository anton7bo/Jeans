program KrimJeansSrv;

uses
  SvcMgr,
  Main in 'Main.pas' {KrimJeans: TService},
  Config in '..\Config.pas',
  Exporter in '..\Exporter.pas',
  Importer in '..\Importer.pas',
  SQLScheme in '..\..\Common\SQLScheme.pas',
  BaseRTTI in '..\..\Common\BaseRTTI.pas',
  Mailer in '..\..\Common\Mailer.pas',
  FibEngine in '..\..\Common\FibEngine.pas',
  LogEngine in '..\..\Common\LogEngine.pas',
  Exchange in '..\..\Common\Exchange.pas',
  MSXML2_TLB in '..\..\Common\MSXML2_TLB.pas',
  drpXMLChecker in '..\drpXMLChecker.pas',
  XMLTransform in '..\..\Common\XMLTransform.pas',
  OldFilesRemover in '..\..\Common\OldFilesRemover.pas',
  BaseExportImport in '..\..\Common\BaseExportImport.pas',
  JnsBaseExportImport in '..\JnsBaseExportImport.pas',
  DRPService in '..\DRPService.pas',
  DRPService5 in '..\DRPService5.pas';

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TKrimJeans, KrimJeans);
  Application.Run;
end.
