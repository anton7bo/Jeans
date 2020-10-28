unit OldFilesRemover;

interface

type
  TOldFilesRemover = class
    FInitPath: string;
    FProcessSubFilders: boolean;
    FDontTouchContentOfInitPath: boolean;
    FStoreFileDays: integer;

    procedure DeleteOldFilesPath(Path: String);
  public
    constructor Create(InitPath: String; StoreFileDays: integer; const ProcessSubFolders: boolean = True; const DontTouchContentOfInitPath: boolean = True);
    procedure DeleteOldFiles;
  end;

implementation

Uses
  SysUtils;

{ TOldFilesRemover }

constructor TOldFilesRemover.Create(InitPath: String; StoreFileDays: integer;
  const ProcessSubFolders, DontTouchContentOfInitPath: boolean);
begin
  FInitPath := InitPath;
  If FInitPath[Length(FInitPath)] <> '\' then FInitPath:=FInitPath + '\';
  FStoreFileDays := StoreFileDays;
  FProcessSubFilders := ProcessSubFolders;
  FDontTouchContentOfInitPath := DontTouchContentOfInitPath;
end;

procedure TOldFilesRemover.DeleteOldFiles;
begin
  try
    DeleteOldFilesPath(FInitPath);
  except
  end;
end;

procedure TOldFilesRemover.DeleteOldFilesPath(Path: String);
var
  sr: TSearchRec;
begin
  If Path[Length(Path)] <> '\' then Path:=Path + '\';

  if FindFirst(Format('%s*.*', [Path]), faAnyFile, sr) = 0 then
  begin
    try
      repeat
        if sr.Attr in [faDirectory] then
        begin
          if (sr.Name = '') or (sr.Name = '.') or (sr.Name = '..') then Continue;

          DeleteOldFilesPath(Path + sr.Name);
          if Abs(Trunc(Date) - Trunc(FileDateToDateTime(sr.Time))) > FStoreFileDays then RemoveDir(Format('%s%s', [Path, sr.Name]));
        end
        else
        begin
          If FDontTouchContentOfInitPath and (Path = FInitPath) then Continue;
          if Abs(Trunc(Date) - Trunc(FileDateToDateTime(sr.Time))) > FStoreFileDays then DeleteFile(Format('%s%s', [Path, sr.Name]));
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

end.
