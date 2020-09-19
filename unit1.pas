unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, Menus,
  win32menustyler;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnTheme1: TButton;
    btnTheme2: TButton;
    btnReset: TButton;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    check2: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem3: TMenuItem;
    check1: TMenuItem;
    disabled1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    normal: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    btnPopup: TButton;
    btnFullScr: TToggleBox;
    procedure btnFullScrChange(Sender: TObject);
    procedure btnPopupClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnTheme1Click(Sender: TObject);
    procedure btnTheme2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOrigWndState: TWindowState;
    FOrigBounds: TRect;
    procedure SetFullScreen(AValue: boolean);
    procedure SetTheme(AColor: TColor);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnTheme1Click(Sender: TObject);
begin
  SetTheme(clGreen);
end;

procedure TForm1.btnTheme2Click(Sender: TObject);
begin
  SetTheme(clNavy);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetTheme(clPurple);
end;

procedure TForm1.btnPopupClick(Sender: TObject);
var
  p: TPoint;
begin
  p:= btnPopup.ClientToScreen(Point(0, btnPopup.Height));
  PopupMenu1.PopUp(p.x, p.y);
end;

procedure TForm1.SetFullScreen(AValue: boolean);
begin
  if AValue then
  begin
    FOrigWndState:= WindowState;
    FOrigBounds:= BoundsRect;
    BorderStyle:= bsNone;
    BoundsRect:= Monitor.BoundsRect;
  end
  else
  begin
    WindowState:= FOrigWndState;
    BoundsRect:= FOrigBounds;
    BorderStyle:= bsSizeable;
    BoundsRect:= FOrigBounds; //again
  end;
end;

procedure TForm1.SetTheme(AColor: TColor);
begin
  Self.Color:= AColor;
  MenuStylerTheme.ColorBk:= AColor;
  MenuStyler.ApplyToForm(Self, true);
  MenuStyler.ApplyToMenu(PopupMenu1);
end;

procedure TForm1.btnFullScrChange(Sender: TObject);
begin
  SetFullScreen(btnFullScr.Checked);
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  Color:= clWindow;
  MenuStyler.ResetForm(Self, true);
  MenuStyler.ResetMenu(PopupMenu1);
end;


end.

