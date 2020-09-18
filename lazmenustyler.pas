{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit lazmenustyler;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows} Windows, {$endif}
  SysUtils, Classes, Graphics, Menus, Forms, LCLType;

type
  TWin32MenuStylerTheme = record
    ColorBk: TColor;
    ColorBkSelected: TColor;
    ColorFont: TColor;
    ColorFontDisabled: TColor;
    CharCheckmark: Widechar;
    CharRadiomark: Widechar;
    FontName: string;
    FontSize: integer;
    IndentX: integer;
    IndentX2: integer;
    IndentY: integer;
  end;

type
  { TWin32MenuStyler }

  TWin32MenuStyler = class
  private
    procedure HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
  public
    procedure ApplyToForm(AForm: TForm);
  end;

var
  MenuStylerTheme: TWin32MenuStylerTheme;
  MenuStyler: TWin32MenuStyler = nil;


implementation

procedure TWin32MenuStyler.ApplyToForm(AForm: TForm);
var
  menu: TMainMenu;
  {$ifdef windows}
  mi: TMENUINFO;
  {$endif}
begin
  menu:= AForm.Menu;
  if menu=nil then exit;

  menu.OwnerDraw:= true;
  menu.OnDrawItem:= @HandleMenuDrawItem;

  {$ifdef windows}
  //this is to theme 2-3 pixel frame around menu popups
  FillChar(mi{%H-}, sizeof(mi), 0);
  mi.cbSize:= sizeof(mi);
  mi.fMask:= MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;
  mi.hbrBack:= CreateSolidBrush(MenuStylerTheme.ColorBk);
  SetMenuInfo(GetMenu(AForm.Handle), @mi);
  {$endif}
end;

procedure TWin32MenuStyler.HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
var
  mi: TMenuItem;
  dx, dy: integer;
  mark: string;
begin
  mi:= Sender as TMenuItem;

  if odSelected in AState then
    ACanvas.Brush.Color:= MenuStylerTheme.ColorBkSelected
  else
    ACanvas.Brush.Color:= MenuStylerTheme.ColorBk;
  ACanvas.FillRect(ARect);

  if mi.IsLine then
  begin
    ACanvas.Pen.Color:= MenuStylerTheme.ColorFontDisabled;
    dx:= MenuStylerTheme.IndentX;
    dy:= (ARect.Top+ARect.Bottom) div 2;
    ACanvas.Line(ARect.Left+dx, dy, ARect.Right-dx, dy);
    exit;
  end;

  if odDisabled in AState then
    ACanvas.Font.Color:= MenuStylerTheme.ColorFontDisabled
  else
    ACanvas.Font.Color:= MenuStylerTheme.ColorFont;

  if mi.IsInMenuBar then
    dx:= MenuStylerTheme.IndentX
  else
    dx:= MenuStylerTheme.IndentX2;
  dy:= MenuStylerTheme.IndentY;

  ACanvas.Font.Name:= MenuStylerTheme.FontName;
  ACanvas.Font.Size:= MenuStylerTheme.FontSize;

  ACanvas.TextOut(ARect.Left+dx, ARect.Top+dy, mi.Caption);

  if mi.Checked then
  begin
    if mi.RadioItem then
      mark:= UnicodeString(MenuStylerTheme.CharRadiomark)
    else
      mark:= UnicodeString(MenuStylerTheme.CharCheckmark);
    ACanvas.TextOut(ARect.Left+2, ARect.Top+dy, mark);
  end;
end;

initialization

  MenuStyler:= TWin32MenuStyler.Create;

  with MenuStylerTheme do
  begin
    ColorBk:= clDkGray;
    ColorBkSelected:= clNavy;
    ColorFont:= clWhite;
    ColorFontDisabled:= clMedGray;
    CharCheckmark:= #$2713;
    CharRadiomark:= #$25CF; //#$2022;
    FontName:= 'default';
    FontSize:= 9;
    IndentX:= 5;
    IndentX2:= 21;
    IndentY:= 2;
  end;

finalization

  FreeAndNil(MenuStyler);

end.
