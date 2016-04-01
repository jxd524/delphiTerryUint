{Copyright:      Hagen Reddmann  mailto:HaReddmann@AOL.COM
 Author:         Hagen Reddmann
 Remarks:        freeware, but this Copyright must be included
 known Problems: none
 Version:        3.0, Delphi Encryption Compendium
                 Delphi 2-4, BCB 3-4, designed and testet under D3 and D4
 Description:    Compression Classes

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}
unit Compress;

interface

uses Classes, DECUtil, ZLib, SysUtils;

type
  TCompress = class(TProtection);

  TCompress_ZLIB = class(TCompress)
  private
  protected
  public
    procedure CodeStream(Source, Dest: TStream; DataSize: Integer; Action: TPAction); override;
  end;

implementation

uses DECConst;

procedure TCompress_ZLIB.CodeStream(Source, Dest: TStream; DataSize: Integer; Action: TPAction);
const
  maxBufSize = 1024 * 4;
var
  S: TCustomZlibStream;
  M: TMemoryStream;
  DPos,MPos: Integer;
  I: Integer;
  Buf: Pointer;
  Ident: array[0..1] of Char;
begin
  if not (Action in Actions) then
  begin
    inherited CodeStream(Source, Dest, DataSize, Action);
    Exit;
  end;
  if Source = nil then Exit;
  if Dest = nil then Dest := Source;
  if DataSize < 0 then
  begin
    DataSize := Source.Size;
    Source.Position := 0;
  end;
  M := TMemoryStream.Create;
  try
    case Action of
     paEncode:
       try
         DPos := Dest.Position;
         M.Write('ZL', 2);
         M.Write(DataSize, SizeOf(DataSize));
         S := TCompressionStream.Create(clMAX, M);
         try
           S.CopyFrom(Source, DataSize);
         finally
           S.Free;
         end;
         M.Position := 0;
         inherited CodeStream(M, M, M.Size, paEncode);
         Dest.Position := DPos + DataSize;
         M.Position := M.Size;
         I := Dest.Size - Dest.Position;
         if I > 0 then M.CopyFrom(Dest, I);
         Dest.Size := DPos;
         M.Position := 0;
         Dest.CopyFrom(M, M.Size);
         Dest.Size := Dest.Position;
       except
         on E: EZLibError do raise EProtection.Create(sInvalidZIPData)
           else raise;
       end;
     paDecode:
       try
         DPos := Dest.Position;
         inherited CodeStream(Source, M, DataSize, paDecode);
         MPos := M.Size;
         M.Position := MPos;
         if Source = Dest then
         begin
           Dest.Position := DPos + DataSize;
           I := Dest.Size - Dest.Position;
           if I > 0 then M.CopyFrom(Dest, I);
         end;
         M.Position := 0;
         M.Read(Ident, SizeOf(Ident));
         M.Read(DataSize, SizeOf(DataSize));
         if (Ident = 'ZL') and (DataSize > 0) then
         begin
           Dest.Position := DPos;
           Dest.Size := DPos;
           S := TDecompressionStream.Create(M);
           Buf := AllocMem(maxBufSize);
           try
             while DataSize > 0 do
             begin
               I := DataSize;
               if I > maxBufSize then I := maxBufSize;
               S.Read(Buf^, I);
               Dest.Write(Buf^, I);
               Dec(DataSize, I);
             end;
           finally
             S.Free;
             ReallocMem(Buf, 0);
           end;
           M.Position := MPos;
           I := M.Size - MPos;
           if I > 0 then Dest.CopyFrom(M, I);
         end else
         begin
           M.Position := 0;
           Dest.Position := DPos;
           Dest.Size := DPos;
           Dest.CopyFrom(M, M.Size);
         end;
         Dest.Size := Dest.Position;
       except
         on E: EZLibError do raise EProtection.Create(sInvalidZIPData)
           else raise;
       end;
    else
      inherited CodeStream(Source, Dest, DataSize, Action);
    end;
  finally
    M.Free;
  end;
end;


end.
