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
    MenuItem2: TMenuItem;
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
    procedure btnTheme1Click(Sender: TObject);
    procedure btnTheme2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  MenuStyler.ApplyToForm(Self, false);
end;

procedure TForm1.btnTheme1Click(Sender: TObject);
begin
  MenuStylerTheme.ColorBk:= clGreen;
  MenuStyler.ApplyToForm(Self, true);
end;

procedure TForm1.btnTheme2Click(Sender: TObject);
begin
  MenuStylerTheme.ColorBk:= clPurple;
  MenuStyler.ApplyToForm(Self, true);
end;

end.

