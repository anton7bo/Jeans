object KrimJeans: TKrimJeans
  OldCreateOrder = False
  DisplayName = 'KrimJeans'
  AfterInstall = ServiceAfterInstall
  AfterUninstall = ServiceAfterUninstall
  OnExecute = ServiceExecute
  OnStop = ServiceStop
  Height = 325
  Width = 434
  object MainTimer: TTimer
    Interval = 60000
    OnTimer = MainTimerTimer
    Left = 56
    Top = 48
  end
  object TelnetServer: TIdTelnetServer
    Bindings = <>
    DefaultPort = 1993
    OnDisconnect = TelnetServerDisconnect
    LoginMessage = 'Indy Telnet Server'
    OnAuthentication = TelnetServerAuthentication
    OnExecute = TelnetServerExecute
    Left = 56
    Top = 112
  end
end
