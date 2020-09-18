{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit win32menustyler;

{$mode objfpc}{$H+}

interface

uses
  Windows, SysUtils, Classes, Graphics, Menus, Forms,
  Types, LCLType;

type
  TWin32MenuStylerTheme = record
    ColorBk: TColor;
    ColorBkSelected: TColor;
    ColorFont: TColor;
    ColorFontDisabled: TColor;
    CharCheckmark: WideChar;
    CharRadiomark: WideChar;
    FontName: string;
    FontSize: integer;
  end;

type
  { TWin32MenuStyler }

  TWin32MenuStyler = class
  private
    procedure HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
  public
    procedure ApplyToMenu(AMenu: TMenu);
    procedure ApplyToForm(AForm: TForm; ARepaintEntireForm: boolean);
    procedure ResetMenu(AMenu: TMenu);
    procedure ResetForm(AForm: TForm; ARepaintEntireForm: boolean);
  end;

var
  MenuStylerTheme: TWin32MenuStylerTheme;
  MenuStyler: TWin32MenuStyler = nil;


implementation

procedure TWin32MenuStyler.ApplyToMenu(AMenu: TMenu);
{
var
  mi: TMENUINFO;
  }
begin
  AMenu.OwnerDraw:= true;
  AMenu.OnDrawItem:= @HandleMenuDrawItem;

  {//it dont work!
  //this is to theme 2-3 pixel frame around menu popups
  FillChar(mi{%H-}, sizeof(mi), 0);
  mi.cbSize:= sizeof(mi);
  mi.fMask:= MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;
  mi.hbrBack:= CreateSolidBrush(MenuStylerTheme.ColorBk);
  SetMenuInfo(AMenu.Handle, @mi);
  }
end;

procedure TWin32MenuStyler.ApplyToForm(AForm: TForm; ARepaintEntireForm: boolean);
var
  menu: TMainMenu;
  mi: TMENUINFO;
begin
  menu:= AForm.Menu;
  if menu=nil then exit;

  ApplyToMenu(menu);

  //this is to theme 2-3 pixel frame around menu popups
  FillChar(mi{%H-}, sizeof(mi), 0);
  mi.cbSize:= sizeof(mi);
  mi.fMask:= MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;
  mi.hbrBack:= CreateSolidBrush(MenuStylerTheme.ColorBk);
  SetMenuInfo(GetMenu(AForm.Handle), @mi);

  //repaint the menu bar
  if ARepaintEntireForm then
    with AForm do
    begin
      Width:= Width+1;
      Width:= Width-1;
    end;
end;

procedure TWin32MenuStyler.ResetMenu(AMenu: TMenu);
begin
  AMenu.OwnerDraw:= false;
  AMenu.OnDrawItem:= nil;
end;

procedure TWin32MenuStyler.ResetForm(AForm: TForm; ARepaintEntireForm: boolean);
var
  menu: TMenu;
  mi: TMENUINFO;
begin
  menu:= AForm.Menu;
  if menu=nil then exit;

  ResetMenu(menu);

  //this is to theme 2-3 pixel frame around menu popups
  FillChar(mi{%H-}, sizeof(mi), 0);
  mi.cbSize:= sizeof(mi);
  mi.fMask:= MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;
  mi.hbrBack:= 0;
  SetMenuInfo(GetMenu(AForm.Handle), @mi);

  //repaint the menu bar
  if ARepaintEntireForm then
    with AForm do
    begin
      Width:= Width+1;
      Width:= Width-1;
    end;
end;

procedure TWin32MenuStyler.HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
const
  cSampleShort = '0';
  cSampleBig = 'Wj';
var
  mi: TMenuItem;
  dx1, dx2, dxMin, Y: integer;
  mark: WideChar;
  BufW: UnicodeString;
  Ext1, Ext2: Types.TSize;
begin
  mi:= Sender as TMenuItem;

  if odSelected in AState then
    ACanvas.Brush.Color:= MenuStylerTheme.ColorBkSelected
  else
    ACanvas.Brush.Color:= MenuStylerTheme.ColorBk;
  ACanvas.FillRect(ARect);

  Windows.GetTextExtentPoint(ACanvas.Handle, PChar(cSampleShort), Length(cSampleShort), Ext1);
  dx1:= Ext1.cx;
  dxMin:= dx1 div 3;

  if mi.IsLine then
  begin
    ACanvas.Pen.Color:= MenuStylerTheme.ColorFontDisabled;
    Y:= (ARect.Top+ARect.Bottom) div 2;
    ACanvas.Line(ARect.Left+dxMin, Y, ARect.Right-dxMin, Y);
    exit;
  end;

  if odDisabled in AState then
    ACanvas.Font.Color:= MenuStylerTheme.ColorFontDisabled
  else
    ACanvas.Font.Color:= MenuStylerTheme.ColorFont;

  ACanvas.Font.Name:= MenuStylerTheme.FontName;
  ACanvas.Font.Size:= MenuStylerTheme.FontSize;
  ACanvas.Font.Style:= [];

  BufW:= UTF8Decode(mi.Caption);
  Windows.GetTextExtentPoint(ACanvas.Handle, PChar(cSampleBig), Length(cSampleBig), Ext2);

  if mi.IsInMenuBar then
    dx2:= dx1
  else
    dx2:= Ext2.cx;

  Y:= (ARect.Top+ARect.Bottom-Ext2.cy) div 2;
  Windows.TextOutW(ACanvas.Handle, ARect.Left+dx2, Y, PWideChar(BufW), Length(BufW));

  if mi.Checked then
  begin
    if mi.RadioItem then
      mark:= MenuStylerTheme.CharRadiomark
    else
      mark:= MenuStylerTheme.CharCheckmark;
    Windows.TextOutW(ACanvas.Handle, ARect.Left+dxMin, Y, @mark, 1);
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
  end;

finalization

  FreeAndNil(MenuStyler);

end.
