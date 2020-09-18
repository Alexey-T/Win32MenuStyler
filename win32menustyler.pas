{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit win32menustyler;

{$mode objfpc}{$H+}

interface

uses
  Windows, SysUtils, Classes, Graphics, Menus, Forms, LCLType;

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
  mi: TMENUINFO;
begin
  menu:= AForm.Menu;
  if menu=nil then exit;

  menu.OwnerDraw:= true;
  menu.OnDrawItem:= @HandleMenuDrawItem;

  //this is to theme 2-3 pixel frame around menu popups
  FillChar(mi{%H-}, sizeof(mi), 0);
  mi.cbSize:= sizeof(mi);
  mi.fMask:= MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;
  mi.hbrBack:= CreateSolidBrush(MenuStylerTheme.ColorBk);
  SetMenuInfo(GetMenu(AForm.Handle), @mi);
end;

procedure TWin32MenuStyler.HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
var
  mi: TMenuItem;
  dx, dy: integer;
  mark: Widechar;
  BufW: UnicodeString;
begin
  mi:= Sender as TMenuItem;

  if odDisabled in AState then
    ACanvas.Font.Color:= MenuStylerTheme.ColorFontDisabled
  else
    ACanvas.Font.Color:= MenuStylerTheme.ColorFont;

  if odSelected in AState then
    ACanvas.Brush.Color:= MenuStylerTheme.ColorBkSelected
  else
    ACanvas.Brush.Color:= MenuStylerTheme.ColorBk;

  if mi.IsInMenuBar then
    dx:= MenuStylerTheme.IndentX
  else
    dx:= MenuStylerTheme.IndentX2;
  dy:= MenuStylerTheme.IndentY;

  ACanvas.FillRect(ARect);
  ACanvas.Font.Name:= MenuStylerTheme.FontName;
  ACanvas.Font.Size:= MenuStylerTheme.FontSize;

  BufW:= UTF8Decode(mi.Caption);
  Windows.TextOutW(ACanvas.Handle, ARect.Left+dx, ARect.Top+dy, PWideChar(BufW), Length(BufW));

  if mi.Checked then
  begin
    if mi.RadioItem then
      mark:= MenuStylerTheme.CharRadiomark
    else
      mark:= MenuStylerTheme.CharCheckmark;
    Windows.TextOutW(ACanvas.Handle, ARect.Left+2, ARect.Top+dy, @mark, 1);
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
