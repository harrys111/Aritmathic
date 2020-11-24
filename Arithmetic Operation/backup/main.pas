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
  ChannelDecimal = record
    R, G, B: Real;
  end;

type
  BitmapColor = array [0..400, 0..400] of Channel;

type
  BitmapBinary = array [0..400, 0..400] of Boolean;

type
  BitmapGrayscale = array [0..400, 0..400] of Byte;

type
  Kernel = array[-1..1, -1..1] of Real;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonExecute: TButton;
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
    procedure ButtonExecuteClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ImagePatternClick(Sender: TObject);
    procedure ImageResultClick(Sender: TObject);
    procedure ImageBackgroundClick(Sender: TObject);
    procedure ImageTextureClick(Sender: TObject);
  private
    procedure BinarizationPattern();
    procedure GrayscalingPattern();
    procedure GrayscalingBackground();
    procedure MergeTexture();
    function InversBinaryImage(Binary: BitmapBinary): BitmapBinary;
    function BoolToByte(value: Boolean):byte;
    function Jikalau(value: integer):byte;
    procedure EdgeRobertBitmapBackground(); // using Robert Operator
    procedure PatternMultiplyTexture();
    procedure PatternTextureAddBackground();
    function PaddingBitmap(bitmap: BitmapColor): BitmapColor;
    function PaddingBitmap(bitmap: BitmapGrayscale): BitmapGrayscale;
    function Filtering(bitmap: BitmapColor; K: Kernel): BitmapColor;

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
   BitmapPattern, BitmapTexture, BitmapBackground, BitmapPatternMultiplyTexture, EmboseBitmapTexture, BitmapResult: BitmapColor;
   BitmapPatternGrayscale, BitmapBackgroundGrayscale, BitmapBackgroundEdge, PaddedBitmapBackground: BitmapGrayscale;
   BitmapPatternBinary, BitmapPatternDilation, BitmapPatternErosion: BitmapBinary;
   imageWidth, imageHeight: Integer;
   LPFKernel: array[-1..1, -1..1] of Real = ((1/9, 1/9, 1/9), (1/9, 1/9, 1/9), (1/9, 1/9, 1/9));
   HPFKernel: array[-1..1, -1..1] of Real = ((-1, -1, -1), (-1, 9, -1), (-1, -1, -1));


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
  GrayscalingPattern();
  BinarizationPattern();
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
  EmboseBitmapTexture:= Filtering(BitmapTexture, HPFKernel);
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
  GrayscalingBackground();
end;

procedure TFormMain.ButtonExecuteClick(Sender: TObject);
var
  x, y: Integer;
begin
  ImageResult.Width:= imageWidth;
  ImageResult.Height:= imageHeight;
  PatternMultiplyTexture();
  EdgeRobertBitmapBackground();
  PatternTextureAddBackground();
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      with EmboseBitmapTexture[x, y] do
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

procedure TFormMain.GrayscalingPattern();
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

procedure TFormMain.GrayscalingBackground();
var
  x, y: Integer;
begin
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      with BitmapBackground[x, y] do
      begin
        BitmapBackgroundGrayscale[x, y]:= (R + G + B) div 3;
      end;
    end;
  end;
end;

procedure TFormMain.BinarizationPattern();
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
      pixelTexture:= EmboseBitmapTexture[x, y];

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
      gray:= BitmapBackgroundEdge[x, y];
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

procedure TFormMain.EdgeRobertBitmapBackground();
var
  ResultBitmap: BitmapGrayscale;
  grayX, grayY: Integer;
  gray: Integer;
  x, y, kx, ky: Integer;
  robertX: array[-1..1, -1..1] of Integer = ((1, 0, 0), (0, -1, 0), (0, 0, 0));
  robertY: array[-1..1, -1..1] of Integer = ((0, -1, 0), (1, 0, 0), (0, 0, 0));
begin
  PaddedBitmapBackground:= PaddingBitmap(BitmapBackgroundGrayscale);
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      grayX:= 0;
      grayY:= 0;
      for ky:= -1 to 1 do
      begin
        for kx:= -1 to 1 do
        begin
          grayX:= grayX + (PaddedBitmapBackground[x-kx, y-ky] * robertX[kx, ky]);
          grayY:= grayY + (PaddedBitmapBackground[x-kx, y-ky] * robertY[kx, ky]);
        end;
      end;
      gray:= 0;
      if grayX > grayY then
        gray:= grayX
      else
        gray:= grayY;
      ResultBitmap[x, y]:= Jikalau(Round(gray));
    end;
  end;
  BitmapBackgroundEdge:= ResultBitmap;
end;

function TFormMain.PaddingBitmap(bitmap: BitmapColor): BitmapColor;
var
  x, y: Integer;
  BitmapTemp: BitmapColor;
begin
  BitmapTemp:= bitmap;
  for y:= 1 to imageHeight do
  begin
    BitmapTemp[0, y]:= bitmap[1, y];
    BitmapTemp[imageWidth+1, y]:= bitmap[imageWidth, y];
  end;

  for x:= 0 to imageWidth+1 do
  begin
    BitmapTemp[x, 0]:= BitmapTemp[x, 1];
    BitmapTemp[x, imageHeight+1]:= BitmapTemp[x, imageHeight];
  end;
  PaddingBitmap:= BitmapTemp;
end;

function TFormMain.PaddingBitmap(bitmap: BitmapGrayscale): BitmapGrayscale;
var
  x, y: Integer;
  BitmapTemp: BitmapGrayscale;
begin
  BitmapTemp:= bitmap;
  for y:= 1 to imageHeight do
  begin
    BitmapTemp[0, y]:= bitmap[1, y];
    BitmapTemp[imageWidth+1, y]:= bitmap[imageWidth, y];
  end;

  for x:= 0 to imageWidth+1 do
  begin
    BitmapTemp[x, 0]:= BitmapTemp[x, 1];
    BitmapTemp[x, imageHeight+1]:= BitmapTemp[x, imageHeight];
  end;
  PaddingBitmap:= BitmapTemp;
end;

function TFormMain.Filtering(bitmap: BitmapColor; K: Kernel): BitmapColor;
var
  x, y: Integer;
  kx, ky: Integer;
  ResultBitmap: BitmapColor;
  pixel: ChannelDecimal;
  padBitmap: BitmapColor;
begin
  padBitmap:= PaddingBitmap(bitmap);
  for y:= 1 to imageHeight do
  begin
    for x:= 1 to imageWidth do
    begin
      pixel.R:= 0;
      pixel.G:= 0;
      pixel.B:= 0;
      for ky:= -1 to 1 do
      begin
        for kx:= -1 to 1 do
        begin
          pixel.R:= pixel.R + (padBitmap[x-kx, y-ky].R * K[kx, ky]);
          pixel.G:= pixel.G + (padBitmap[x-kx, y-ky].G * K[kx, ky]);
          pixel.B:= pixel.B + (padBitmap[x-kx, y-ky].B * K[kx, ky]);
        end;
      end;
      ResultBitmap[x, y].R:= Jikalau(Round(pixel.R));
      ResultBitmap[x, y].G:= Jikalau(Round(pixel.G));
      ResultBitmap[x, y].B:= Jikalau(Round(pixel.B));
    end;
  end;
  Filtering:= ResultBitmap;
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

