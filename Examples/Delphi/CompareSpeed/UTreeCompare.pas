unit UTreeCompare;
{$I TeeDefs.inc}

interface

{
  This example compares the speed of adding nodes of:

  TeeTree TTree
  VCL TTreeView
  VirtualTreeView
}

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,

  {$DEFINE VIRTUAL_TreeView} // <-- Comment this line to avoid VirtualTreeView usage

  {$IFDEF VIRTUAL_TreeView}
  VirtualTrees, VirtualTrees.BaseTree, VirtualTrees.Types,
  {$ENDIF}

  TeeProcs, TeCanvas, TeeTree;

{ This project compares the speed of TeeTree versus TreeView }

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label5: TLabel;
    Label6: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button5: TButton;
    CheckBox3: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }

    Tree1 : TTree;

    {$IFDEF VIRTUAL_TreeView}
    VST: TVirtualStringTree;

    function AddVirtualTreeView(NumNodes:Integer):Integer;
    function AddVirtualTreeViewLevels(NumLevels,NodesPerLevel:Integer):Integer;

    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                    Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
                        var InitialStates: TVirtualNodeInitStates);

    {$ENDIF}

    procedure InitTesting;

    function AddTeeTree(NumNodes:Integer):Integer;
    function AddTeeTreeLevels(NumLevels,NodesPerLevel:Integer):Integer;

    function AddTreeView(NumNodes:Integer):Integer;
    function AddTreeViewLevels(NumLevels,NodesPerLevel:Integer):Integer;

    procedure Test(NumNodes:Integer);
    procedure TestLevels(NumLevels,NodesPerLevel:Integer);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF LCL}
{$R *.DFM}
{$ELSE}
{$R *.lfm}
{$ENDIF}

// Just cosmetic labels
procedure TForm1.InitTesting;
begin
  if CheckBox1.Checked then
     Label5.Caption:='Testing...'
  else
     Label5.Caption:='(not tested)';

  if CheckBox2.Checked then
     Label6.Caption:='Testing...'
  else
     Label6.Caption:='(not tested)';

  if CheckBox3.Checked then
     Label3.Caption:='Testing...'
  else
     Label3.Caption:='(not tested)';

  Label3.Update;
  Label5.Update;
  Label6.Update;
end;

procedure TForm1.Test(NumNodes:Integer);
begin
  Screen.Cursor:=crHourGlass;
  try
    InitTesting;

    if CheckBox1.Checked then
    begin
      Label5.Caption:=IntToStr(AddTeeTree(NumNodes))+' msec';
      Label5.Update;
    end;

    if CheckBox2.Checked then
    begin
      Label6.Caption:=IntToStr(AddTreeView(NumNodes))+' msec';
      Label6.Update;
    end;

    if CheckBox3.Checked then
    begin
      {$IFDEF VIRTUAL_TreeView}
      Label3.Caption:=IntToStr(AddVirtualTreeView(NumNodes))+' msec';
      Label3.Update;
      {$ENDIF}
    end;

  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Test(5000);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Test(10000);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Test(50000);
end;

function TForm1.AddTeeTree(NumNodes:Integer):Integer;
var t1,t2,t:Integer;
begin
  Tree1.Clear;

  t1:=GetTickCount;

  // add nodes
  Tree1.BeginUpdate;

  with Tree1.AddRoot('Root') do
  for t:=1 to NumNodes do AddChild('Node '+IntToStr(t));

  Tree1.EndUpdate;

  t2:=GetTickCount;

  Tree1.Roots[0].Expanded:=True;

  result:=t2-t1;
end;

function TForm1.AddTeeTreeLevels(NumLevels,NodesPerLevel:Integer):Integer;

  procedure AddNodesTo(const ANode:TTreeNodeShape; const ALevel:Integer);
  var t : Integer;
      tmp : TTreeNodeShape;
  begin
    for t:=1 to NodesPerLevel do
    begin
      tmp:=ANode.AddChild('Node '+IntToStr(Tree1.Items.Count));

      if ALevel<NumLevels then
         AddNodesTo(tmp,ALevel+1);
    end;
  end;

var t1,t2,
    t:Integer;
begin
  Tree1.Clear;

  t1:=GetTickCount;

  Tree1.BeginUpdate;

  for t:=1 to NodesPerLevel do
     AddNodesTo(Tree1.AddRoot('Root'),1);

  Tree1.EndUpdate;

  t2:=GetTickCount;

  Tree1.Roots[0].Expanded:=True;

  result:=t2-t1;
end;

function TForm1.AddTreeView(NumNodes:Integer):Integer;
var t1,t2,t:Integer;
    Root:TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  TreeView1.Items.EndUpdate;

  t1:=GetTickCount;

  TreeView1.Items.BeginUpdate;

  Root:=TreeView1.Items.Add(nil,'Root');

  for t:=1 to NumNodes do
      TreeView1.Items.AddChild(Root,'Node '+IntToStr(t));

  TreeView1.Items.EndUpdate;

  t2:=GetTickCount;

  TreeView1.Items[0].Expanded:=True;

  result:=t2-t1;
end;

function TForm1.AddTreeViewLevels(NumLevels,NodesPerLevel:Integer):Integer;

  procedure AddNodesTo(const ANode:TTreeNode; const ALevel:Integer);
  var t : Integer;
      tmp : TTreeNode;
  begin
    for t:=1 to NodesPerLevel do
    begin
      tmp:=TreeView1.Items.AddChild(ANode,'Node '+IntToStr(TreeView1.Items.Count));

      if ALevel<NumLevels then
         AddNodesTo(tmp,ALevel+1);
    end;
  end;

var t1,t2,t:Integer;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  TreeView1.Items.EndUpdate;

  t1:=GetTickCount;

  TreeView1.Items.BeginUpdate;

  for t:=1 to NodesPerLevel do
     AddNodesTo(TreeView1.Items.Add(nil,'Root'),1);

  TreeView1.Items.EndUpdate;

  t2:=GetTickCount;

  TreeView1.Items[0].Expanded:=True;

  result:=t2-t1;
end;

{$IFDEF VIRTUAL_TreeView}
type
  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: WideString;
  end;

procedure TForm1.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TForm1.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data.Caption;
end;

procedure TForm1.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var Data: PMyRec;
begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    Data.Caption := Format('Level %d, Index %d', [GetNodeLevel(Node), Node.Index]);
  end;
end;

function TForm1.AddVirtualTreeView(NumNodes:Integer):Integer;
var t1,t2 : Integer;
    tmp : PVirtualNode;
begin
  t1:=GetTickCount;

  tmp:=VST.AddChild(nil);
  VST.ChildCount[tmp]:=NumNodes;

  VST.Expanded[tmp] := True;
  VST.InvalidateToBottom(tmp);

  t2:=GetTickCount;
  result:=t2-t1;
end;

function TForm1.AddVirtualTreeViewLevels(NumLevels,NodesPerLevel:Integer):Integer;

  procedure AddNodesTo(const ANode:PVirtualNode; const ALevel:Integer);
  var tmp : PVirtualNode;
  begin
    VST.ChildCount[ANode]:=NodesPerLevel;

    if ALevel<NumLevels then
       for tmp in VST.ChildNodes(ANode) do
           AddNodesTo(tmp,ALevel+1);
  end;

var t1,t2,t : Integer;
    tmp : PVirtualNode;
begin
  t1:=GetTickCount;

  for t:=1 to NodesPerLevel do
     AddNodesTo(VST.AddChild(nil),1);

  tmp:=VST.GetFirstChild(nil);

  VST.Expanded[tmp] := True;
  VST.InvalidateToBottom(tmp);

  t2:=GetTickCount;
  result:=t2-t1;
end;

{$ENDIF}

procedure TForm1.Button4Click(Sender: TObject);
var t1,t2: Integer;
begin
  // Add 10000 nodes so we can Clear them later.
  Test(10000);

  // Clear TeeTree
  t1:=GetTickCount;
  Tree1.Clear;
  t2:=GetTickCount;
  Label5.Caption:=IntToStr(t2-t1)+' msec';

  // Clear TreeView
  t1:=GetTickCount;
  with TreeView1.Items do
  begin
    BeginUpdate;
    Clear;
    EndUpdate;
  end;

  t2:=GetTickCount;
  Label6.Caption:=IntToStr(t2-t1)+' msec';

  {$IFDEF VIRTUAL_TreeView}
  t1:=GetTickCount;
  VST.Clear;
  t2:=GetTickCount;
  Label3.Caption:=IntToStr(t2-t1)+' msec';
  {$ENDIF}
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Close;
end;

// Add NodesPerLevel as root nodes, then add their childs, etc, recursive
procedure TForm1.TestLevels(NumLevels,NodesPerLevel:Integer);
begin
  Screen.Cursor:=crHourGlass;
  try
    InitTesting;

    if CheckBox1.Checked then
    begin
      Label5.Caption:=IntToStr(AddTeeTreeLevels(NumLevels,NodesPerLevel))+' msec';
      Label5.Update;
    end;

    if CheckBox2.Checked then
    begin
      Label6.Caption:=IntToStr(AddTreeViewLevels(NumLevels,NodesPerLevel))+' msec';
      Label6.Update;
    end;

    if CheckBox3.Checked then
    begin
      {$IFDEF VIRTUAL_TreeView}
      Label3.Caption:=IntToStr(AddVirtualTreeViewLevels(NumLevels,NodesPerLevel))+' msec';
      Label3.Update;
      {$ENDIF}
    end;

  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  TestLevels(3,15);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create a TTree manually (it can also be added at design-time)

  Tree1:=TTree.Create(Self);
  Tree1.View3DOptions.ZoomText:=ztManual;
  Tree1.View3DOptions.FontZoom:=116;

  Tree1.SetBounds(208,24,233,321);
  Tree1.CrossBox.Size:=6;
  Tree1.Parent:=Self;

  with Tree1.GlobalFormat do
  begin
    Border.Visible:=False;
    Transparent:=True;
    ImageIndex:=tiNone;
    Connection.ArrowTo.Style:=casNone;
  end;

  {$IFDEF VIRTUAL_TreeView}
  VST:=TVirtualStringTree.Create(Self);

//  VST.BevelInner:=bvRaised;
//  VST.BevelOuter:=bvLowered;

  VST.SetBounds(720,24,233,321);

  VST.NodeDataSize := SizeOf(TMyRec);
  VST.Parent:=Self;

  VST.OnInitNode:=VSTInitNode;
  VST.OnFreeNode:=VSTFreeNode;
  VST.OnGetText:=VSTGetText;
  {$ENDIF}
end;

end.
