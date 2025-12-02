unit Ordering;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TeeTree, TreeFlow, ExtCtrls, TeeProcs;

type
  TOrderingForm = class(TForm)
    Memo1: TMemo;
    Tree1: TTree;
    TreeNodeShape1: TTreeNodeShape;
    TreeNodeShape2: TTreeNodeShape;
    TreeNodeShape3: TTreeNodeShape;
    DocumentShape1: TDocumentShape;
    CallOutShape1: TCallOutShape;
    PolyLineShape1: TPolyLineShape;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

initialization
  RegisterClass(TOrderingForm);
end.
