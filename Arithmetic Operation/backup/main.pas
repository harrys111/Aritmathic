unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ExtDlgs;

type
  Channel = record
    R, G, B: Byte;
  end;

type
  BitmapColor = array [0..400, 0..400] of Channel;

type
  BitmapBinary = array [0..400, 0..400] of Boolean;

type
  BitmapGrayscale = array [0..400, 0..400] of Byte;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonMagic: TButton;
    ButtonSave: TButton;
    ButtonLoadPattern: TButton;
    ButtonLoadTexture: TButton;
    ButtonLoadBackground: TButton;
    ImageBackground: TImage;
    ImageTexture: TImage;
    ImagePattern: TImage;
    ImageResult: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    procedure ButtonLoadPatternClick(Sender: TObject);
    procedure ButtonLoadTextureClick(Sender: TObject);
    procedure ButtonLoadBackgroundClick(Sender: TObject);
    procedure ButtonMagicClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ImagePatternClick(Sender: TObject);
    procedure ImageResultClick(Sender: TObject);
    procedure ImageBackgroundClick(Sender: TObject);
    procedure ImageTextureClick(Sender: TObject);
  private
    procedure Binarization();
    procedure Grayscaling();
    procedure MergeTexture();
    function MergePatternWithTexture(Texture: BitmapColor; Binary: BitmapBinary): BitmapColor;
    function InversBinaryImage(Binary: BitmapBinary): BitmapBinary;
    function BoolToByte(value: Boolean):byte;
    function Jikalau(value: integer):byte;
    procedure PatternMultiplyTexture();
    procedure PatternTextureAddBackground();

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

uses
  windows;

var
   BitmapPattern, BitmapTexture, BitmapBackground, BitmapPatternMultiplyTexture, BitmapResult: BitmapColor;
   BitmapPatternGrayscale: BitmapGrayscale;
   BitmapPatternBinary, BitmapPatternDilation, BitmapPatternErosion: BitmapBinary;
   imageWidth, imageHeight: Integer;


procedure TFormMain.ButtonLoadPatternClick(Sender: TObject);
var
  x, y: Integer;
begin
  if OpenPictureDialog1.Execute then
  begin
    ImagePattern.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    imageWidth:= ImagePattern.Width;
    imageHeight:= ImagePattern.Height;

    ImageBackground.Width:= imageWidth;
    ImageBackground.Height:= imageHeight;

    ImageTexture.Width:= imageWidth;
    ImageTexture.Height:= imageHeight;

    for y:= 1 to imageHeight do
    begin
      for x:= 1 to imageWidth do
      begin
        BitmapPattern[x, y].R:= GetRValue(ImagePattern.Canvas.Pixels[x-1, y-1]);
        BitmapPattern[x, y].G:= GetGValue(ImagePattern.Canvas.Pixels[x-1, y-1]);
        BitmapPattern[x, y].B:= GetBValue(ImagePattern.Canvas.Pixels[x-1, y-1]);
      end;
    end;
  end;
  Grayscaling();
  Binarization();
end;

function TFormMain.BoolToByte(value: Boolean): Byte;
begin
  if value then BoolToByte:= 1 else BoolToByte:= 0;
end;

procedure TFormMain.ButtonLoadTextureClick(Sender: TObject);
var
  x, y: Integer;
begin
  if OpenPictureDialog1.Execute then
  begin
    ImageTexture.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    for y:= 1 to imageHeight do
    begin
      for x:= 1 to imageWidth do
      begin
        BitmapTexture[x, y].R:= GetRValue(ImageTexture.Canvas.Pixels[x-1, y-1]);
        BitmapTexture[x, y].G:= GetGValue(ImageTexture.Canvas.Pixels[x-1, y-1]);
        BitmapTexture[x, y].B:= GetBValue(ImageTexture.Canvas.Pixels[x-1, y-1]);
      end;
    end;
  end;
end;

procedure TFormMain.ButtonLoadBackgroundClick(Sender: TObject);
var
  x, y: Integer;
begin
  if OpenPictureDialog1.Execute then
  begin
    ImageBackground.Picture.LoadFromFile(OpenPictureDialog1.FileName);

    for y:= 1 to imageHeight do
    begin
      for x:= 1 to imageWidth do
      begin
        BitmapBackground[x, y].R:= GetRValue(ImageBackground.Canvas.Pixels[x-1, y-1]);
        BitmapBackground[x, y].G:= GetGValue(ImageBackground.Canvas.Pixels[x-1, y-1]);
        BitmapBackground[x, y].B:= GetBValue(ImageBackground.Canvas.Pixels[x-1, y-1]);
      end;
    end;
  end;
end;

procedure TFormMain.ButtonMagicClick(Sender: TObject);
var
  x, y: Integer;
  gray: Byte;
begin
  ImageResult.Width:= imageWidth;
  ImageResult.Height:= imageHeight;
  PatternMultiplyTexture();
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      with BitmapResult[x, y] do
      begin
        ImageResult.Canvas.Pixels[x-1, y-1]:= RGB(R, G, B);
      end;
    end;
  end;
end;

function TFormMain.Jikalau(value: integer): byte;
begin
  if value < 0 then Jikalau := 0
  else if value > 255 then Jikalau := 255
  else Jikalau := value;
end;

procedure TFormMain.Grayscaling();
var
  x, y: Integer;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      with BitmapPattern[x, y] do
      begin
        BitmapPatternGrayscale[x, y]:= (R + G + B) div 3;
      end;
    end;
  end;
end;

procedure TFormMain.Binarization();
var
  x, y: Integer;
  gray: Byte;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      gray:= BitmapPatternGrayscale[x, y];
      if gray >= 127 then
        BitmapPatternBinary[x, y]:= true
      else
        BitmapPatternBinary[x, y]:= false;
    end;
  end;
end;

procedure TFormMain.PatternMultiplyTexture();
var
  x, y: Integer;
  pixelPattern: Boolean;
  pixelTexture, pixelResult: Channel;
  InversBitmapPatternBinary: BitmapBinary;
begin
  InversBitmapPatternBinary:= InversBinaryImage(BitmapPatternBinary);
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      pixelPattern:= InversBitmapPatternBinary[x, y];
      pixelTexture:= BitmapTexture[x, y];

      pixelResult.R:= BoolToByte(pixelPattern) * pixelTexture.R;
      pixelResult.G:= BoolToByte(pixelPattern) * pixelTexture.G;
      pixelResult.B:= BoolToByte(pixelPattern) * pixelTexture.B;

      BitmapPatternMultiplyTexture[x, y]:= pixelResult;
    end;
  end;
end;

procedure TFormMain.PatternTextureAddBackground();
var
  x, y: Integer;
  pixelPatternTexture, pixelResult: Channel;
  gray: Byte;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      pixelPatternTexture:= BitmapPatternMultiplyTexture[x, y];
      with BitmapBackground[x, y] do
      begin
        gray:= (R + G + B) div 3;
      end;
      pixelResult.R:= Jikalau(pixelPatternTexture.R + gray);
      pixelResult.G:= Jikalau(pixelPatternTexture.G + gray);
      pixelResult.B:= Jikalau(pixelPatternTexture.B + gray);
      BitmapResult[x, y]:= pixelResult;
    end;
  end;
end;

procedure TFormMain.MergeTexture();
begin

end;

function TFormMain.MergePatternWithTexture(Texture: BitmapColor; Binary: BitmapBinary): BitmapColor;
var
  x, y: Integer;
  BitmapTemp: BitmapColor;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      if Binary[x, y] then
      begin
        BitmapTemp[x, y].R:= Texture[x, y].R * 1;
        BitmapTemp[x, y].G:= Texture[x, y].G * 1;
        BitmapTemp[x, y].B:= Texture[x, y].B * 1;
      end
      else
      begin
        BitmapTemp[x, y].R:= 0;
        BitmapTemp[x, y].G:= 0;
        BitmapTemp[x, y].B:= 0;
      end;
    end;
  end;
  MergePatternWithTexture:= BitmapTemp;
end;

function TFormMain.InversBinaryImage(Binary: BitmapBinary): BitmapBinary;
var
  x, y: Integer;
  BitmapTemp: BitmapBinary;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      BitmapTemp[x, y]:= not Binary[x, y];
    end;
  end;
  InversBinaryImage:= BitmapTemp;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
  begin
    ImageResult.Picture.SaveToFile(SavePictureDialog1.FileName);
  end;
end;

procedure TFormMain.ImagePatternClick(Sender: TObject);
begin

end;

procedure TFormMain.ImageResultClick(Sender: TObject);
begin

end;

procedure TFormMain.ImageBackgroundClick(Sender: TObject);
begin

end;

procedure TFormMain.ImageTextureClick(Sender: TObject);
begin

end;

end.

